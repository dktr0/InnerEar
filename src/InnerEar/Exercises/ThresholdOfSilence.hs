{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.ThresholdOfSilence (thresholdOfSilenceExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic
import Reflex.Synth.Types

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.Data
import InnerEar.Widgets.Config
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.SpecEval
import InnerEar.Widgets.AnswerButton
import InnerEar.Widgets.Utility



type Config = Int -- gain value for attenuated sounds

configs :: [Config]
configs = [-20,-30,-40,-50,-60,-70,-80,-90,-100,-110]

configMap::Map Int (String, Config)
configMap = fromList $ zip [(0::Int),1..]$ fmap (\x-> (show x ++ " dB",x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

answers :: [Answer]
answers = [Answer True,Answer False]

instance Show Answer where
  show (Answer True) = "Attenuated Sound"
  show (Answer False) = "No sound at all"

instance Buttonable Answer where
  makeButton = showAnswerButton


renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db s (Just (Answer True)) = GainSound (Sound s)  ((fromIntegral db)::Double) -- 2.0 -- should be a sound source attenuated by dB value
renderAnswer db _ (Just (Answer False)) = NoSound -- 2.0
-- renderAnswer db _ Nothing = GainSound (OverlappedSound "test" oscs) (-20)
--   where oscs = fmap (\x -> DelayedSound (Sound $ NodeSource (OscillatorNode $ Oscillator Sine x 0) (Just 0.5)) (x/100) ) [100::Double,200,300,400,500,600,700,800]
-- renderAnswer db _ Nothing
renderAnswer db s (Nothing) = GainSound (Sound s) ((fromIntegral db)::Double)

instructionsText = "In this exercise, the system either makes no sound at all \
    \or it plays a sound that has been reduced in level by some specific amount \
    \of attenuation. As you make the level lower and lower, it should become more \
    \difficult to tell when the system is playing a sound versus when it is playing \
    \nothing."

data MyTabs = Instructions | Other deriving (Eq,Show)

instructions :: MonadWidget t m => m ()
-- instructions = elClass "div" "instructionsText" $ text instructionsText
instructions = elClass "div" "instructionsText" $ do
  tab <- simpleTabBar [Instructions,Other] Instructions
  tabAVisible <- mapDyn (== Instructions) tab
  visibleWhen tabAVisible $ text "This would be the text of the instructions."
  tabBVisible <- mapDyn (== Other) tab
  visibleWhen tabBVisible $ text "Now we are displaying some other text instead!"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers


sourcesMap:: Map Int (String,Source)
sourcesMap = fromList $ zip [0::Int,1..] [("Pink noise",NodeSource (BufferNode $ File "pinknoise.wav") (Just 2)), ("White noise", NodeSource (BufferNode $ File "whitenoise.wav") (Just 2)), ("Load a soundfile", NodeSource (BufferNode $ LoadedFile "thresholdOfSilenceExercise" (PlaybackParam 0 1 False)) Nothing)]

thresholdOfSilenceExercise :: MonadWidget t m => Exercise t m Int [Answer] Answer (Map Answer Score) s
thresholdOfSilenceExercise = multipleChoiceExercise
  1
  answers
  instructions
  (configWidget "thresholdOfSilenceExercise" sourcesMap 0 "Attenuation:  " configMap) -- (dynRadioConfigWidget "fiveBandBoostCutExercise" sourcesMap 0  configMap)
  renderAnswer
  ThresholdOfSilence
  (-20)
  displayEval
  generateQ

{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.ThresholdOfSilence (thresholdOfSilenceExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Text.JSON
import Text.JSON.Generic
import Reflex.Synth.Synth

import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Sound
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Widgets.AnswerButton

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

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec, Maybe Time) -> Maybe Answer -> Synth ()
renderAnswer _ db (src, dur) (Just (Answer True)) = buildSynth $ do
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ fromIntegral db) >> env >> destination
  maybeDelete (fmap (+ Sec 0.2) dur)
renderAnswer _ db _ (Just (Answer False)) = return ()
renderAnswer _ db (src, dur) Nothing = buildSynth $ do
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ fromIntegral db) >> env >> destination
  maybeDelete (fmap (+ Sec 0.2) dur)

instructionsText = "In this exercise, the system either makes no sound at all \
    \or it plays a sound that has been reduced in level by some specific amount \
    \of attenuation. As you make the level lower and lower, it should become more \
    \difficult to tell when the system is playing a sound versus when it is playing \
    \nothing."

instructions :: MonadWidget t m => m ()
instructions = elClass "div" "instructionsText" $ text instructionsText

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer], Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

sourcesMap :: Map Int (String, SoundSourceConfigOption)
sourcesMap = fromList $ [
    (0, ("Pink noise", Resource "pinknoise.wav" (Just $ Sec 2))),
    (1, ("White noise", Resource "whitenoise.wav" (Just $ Sec 2))),
    (2, ("Load a sound file", UserProvidedResource))
  ]

thresholdOfSilenceExercise :: MonadWidget t m => Exercise t m Int [Answer] Answer (Map Answer Score)
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

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
import InnerEar.Types.Data (Datum)
import InnerEar.Widgets.Config
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.SpecEval



type Config = Int -- gain value for attenuated sounds

configs :: [Config]
configs = [-20,-30,-40,-50,-60,-70,-80,-90,-100,-110]

configMap::Map String Config
configMap = fromList $ fmap (\x-> (show x ++ " dB",x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

answers :: [Answer]
answers = [Answer True,Answer False]

instance Show Answer where
  show (Answer True) = "Attenuated Sound"
  show (Answer False) = "No sound at all"

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db s (Just (Answer True)) = GainSound (Sound s)  ((fromIntegral db)::Double) -- 2.0 -- should be a sound source attenuated by dB value
renderAnswer db _ (Just (Answer False)) = NoSound -- 2.0
renderAnswer db s (Nothing) = GainSound (Sound s) ((fromIntegral db)::Double)

thresholdOfSilenceConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
thresholdOfSilenceConfigWidget i = radioConfigWidget explanation msg configs i
  where
    explanation = "In this exercise, the system either makes no sound at all or it plays a sound that has been reduced in level by some specific amount of attenuation. As you make the level lower and lower, it should become more difficult to tell when the system is playing a sound versus when it is playing nothing. You can configure how much attenuation to apply below, then click Begin Exercise to begin. If it is too easy, return to this configuration page and select a more extreme level of attenuation (i.e. an even lower gain value in decibels)."
    msg = "Please choose the level of attenuation for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

thresholdOfSilenceExercise :: MonadWidget t m => Exercise t m Int [Answer] Answer (Map Answer Score)
thresholdOfSilenceExercise = multipleChoiceExercise
  1
  answers
  (return ())
  (dynRadioConfigWidget "thersholdOfSilence" configMap)
  renderAnswer
  ThresholdOfSilence
  (-20)
  thresholdOfSilenceConfigWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")

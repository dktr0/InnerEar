{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.AddedWhiteNoise (addedWhiteNoiseExercise) where

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
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data (Datum)

type Config = Double -- representing level of attenuation for added white noise

configs :: [Config]
configs = [-10,-20,-30,-40,-50,-60,-65,-70,-75,-80]

configMap:: Map String Config
configMap = fromList $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "With Noise"
  show (Answer False) = "Clean"

answers = [Answer False,Answer True]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db (NodeSource node dur) (Just (Answer True)) = OverlappedSound "addedWhiteNoiseExercise"  [GainSound (Sound $ NodeSource node dur) (-10) , GainSound (Sound $ NodeSource (BufferNode $ File "whitenoise.wav") dur ) db] -- should be soundSource (b) at -10 plus whiteNoise at dB
renderAnswer db b (Just (Answer False)) = OverlappedSound "addedWhiteNoiseExercise" [GainSound (Sound b) (-10)] -- note: this must be an overlapped sound so that it cuts off the previous playing sound...
renderAnswer db b Nothing = OverlappedSound "addedWhiteNoiseExercise" [GainSound (Sound b) (-10)] -- should also just be soundSource (b) at -10
-- note also: default sound source for this is a 300 Hz sine wave, but user sound files are possible
-- pink or white noise should NOT be possible as selectable sound source types

configurationWidget :: MonadWidget t m => Config -> m (Event t Config)
configurationWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the level of (potential) added white noise for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, a low level of noise (white noise) is potentially added to a reference signal. Your task is to detect whether or not the noise has been added. Configure the level of the noise progressively lower and lower to challenge yourself."
  elClass "div" "instructionsText" $ text "Note: the exercise will work right away with a sine wave as a reference tone (to which noise is or is not added), however it is strongly recommended that the exercise be undertaken with recorded material such as produced music, field recordings, etc. Click on the sound source menu to load a sound file from the local filesystem."

addedWhiteNoiseExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
addedWhiteNoiseExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  instructions
  (dynRadioConfigWidget' "addedWhiteNoiseExercise" configMap)
  renderAnswer
  AddedWhiteNoise
  (-10)
  configurationWidget
  displayEval
  generateQ

{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.AddedWhiteNoise (addedWhiteNoiseExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Data.Maybe
import Text.JSON
import Text.JSON.Generic

import Reflex.Synth.Synth
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Sound
import InnerEar.Widgets.AnswerButton

type Config = Double -- representing level of attenuation for added white noise

configs :: [Config]
configs = [-10,-20,-30,-40,-50,-60,-65,-70,-75,-80]

configMap:: Map Int (String,Config)
configMap = fromList $ zip [0::Int,1..] $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "With Noise"
  show (Answer False) = "Clean"

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [Answer False,Answer True]

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec, Maybe Time) -> Maybe Answer -> Synth ()
renderAnswer sysResources db (src, dur) (Just (Answer True)) = buildSynth $ do
  -- sticking an EmptyGraph somewhere won't break the stack structure of Synths right?
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ fromIntegral $ -10) >> env >> destination
  audioBufferSource (sysResources!"whitenoise.wav") (BufferParams 0 1 (isJust dur)) >> gain (Db db) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)
renderAnswer _ db (src, dur) _ = buildSynth $ do
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ fromIntegral $ -10) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)


displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, a low level of noise (white noise) is potentially added to a reference signal. Your task is to detect whether or not the noise has been added. Configure the level of the noise progressively lower and lower to challenge yourself."
  elClass "div" "instructionsText" $ text "Note: the exercise will work right away with a sine wave as a reference tone (to which noise is or is not added), however it is strongly recommended that the exercise be undertaken with recorded material such as produced music, field recordings, etc. Click on the sound source menu to load a sound file from the local filesystem."


-- Change to SoundSourceConfigOption instead of Source
sourcesMap :: Map Int (String, SoundSourceConfigOption)
sourcesMap = fromList $ [
    (0, ("300hz sine wave", Spec (Oscillator Sine (Hz 300)) (Just $ Sec 2))), 
    (1, ("Load a sound file", UserProvidedResource))
  ]


addedWhiteNoiseExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
addedWhiteNoiseExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  instructions
  (configWidget "addedWhiteNoiseExercise" sourcesMap 0 "Noise level (dB): " configMap)
  renderAnswer
  AddedWhiteNoise
  (-10)
  displayEval
  generateQ

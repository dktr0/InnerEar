{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.HarmonicDistortion (harmonicDistortionExercise) where

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
import InnerEar.Types.Data
import InnerEar.Widgets.AnswerButton

type Config = Double -- representing threshold of clipping, and inverse of post-clip normalization

configs :: [Config]
configs = [-12,-6,-3,-2,-1,-0.75,-0.5,-0.25,-0.1,-0.05]

configMap:: Map Int (String,Config)
configMap = fromList $ zip [0::Int,1..] $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Clipped"
  show (Answer False) = "Not Clipped"

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [Answer False,Answer True]
--
-- renderAnswer :: Config -> b -> Maybe Answer -> Sound
-- renderAnswer db _ (Just (Answer True)) = GainSound (ProcessedSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (DistortAtDb db)) (-10) -- 2.0 -- should be a sine wave clipped and normalized by db, then attenuated a standard amount (-10 dB)
-- renderAnswer db _ (Just (Answer False)) =  GainSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (-10) -- 2.0 -- should be a clean sine wave, just attenuated a standard amount (-10 dB)
-- renderAnswer db _ Nothing =  GainSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (-10)

renderAnswer :: Config -> b -> Maybe Answer -> Sound
renderAnswer db _ (Just (Answer True)) = GainSound (WaveShapedSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (ClipAt db)) (-10) -- 2.0 -- should be a sine wave clipped and normalized by db, then attenuated a standard amount (-10 dB)
renderAnswer db _ (Just (Answer False)) =  GainSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (-10) -- 2.0 -- should be a clean sine wave, just attenuated a standard amount (-10 dB)
renderAnswer db _ Nothing =  GainSound (Sound $ NodeSource  (OscillatorNode $ Oscillator Sine 300 0) (Just 2)) (-10)

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph ("scoreBarWrapper","svgBarContainer","svgFaintedLine", "xLabel") "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, each question will sound either as a pure tone or as a clipped version of that tone."
  elClass "div" "instructionsText" $ text "You can configure the exercise to have different levels of clipping. For example, at the -10 dB setting any clipped sound will be clipped 10 dB below its peak (extreme clipping) resulting in a markedly brighter, buzzier sound. At the -0.5 dB setting, the signal will be clipped just 0.5 dB below its peak, resulting in a more subtle difference from the unclipped alternative."



sourcesMap:: Map Int (String,Source)
sourcesMap = fromList $ [(0,("300hz sine wave", NodeSource (OscillatorNode $ Oscillator Sine 440 0) (Just 2)))]

harmonicDistortionExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
harmonicDistortionExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  instructions
  (configWidget "harmonicDistortionExercise" sourcesMap 0 "Clip level: " configMap) -- (dynRadioConfigWidget "fiveBandBoostCutExercise" sourcesMap 0  configMap)
  renderAnswer
  HarmonicDistortion
  (-12)
  displayEval
  generateQ

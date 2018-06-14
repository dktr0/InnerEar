{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.HarmonicDistortion (harmonicDistortionExercise) where

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
import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data hiding (Time)
import InnerEar.Types.Sound
import InnerEar.Widgets.AnswerButton

type Config = Double -- representing threshold of clipping, and inverse of post-clip normalization

configs :: [Config]
configs = [-12,-6,-3,-2,-1,-0.75,-0.5,-0.25,-0.1,-0.05]

configMap:: Map Int (String, Config)
configMap = fromList $ zip [0::Int,1..] $ fmap (\x-> (show x++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Clipped"
  show (Answer False) = "Not Clipped"

instance Buttonable Answer where
  makeButton = showAnswerButton

answers = [Answer False,Answer True]

renderAnswer :: Map String AudioBuffer -> Config -> (SourceNodeSpec, Maybe Time)-> Maybe Answer -> Synth ()
renderAnswer _ clipDb (src,dur) (Just (Answer True)) = buildSynth $ do
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> clipAt (Db clipDb) >> gain (Db $ fromIntegral $ -10) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)
renderAnswer _ clipDb (src,dur) _ = buildSynth $ do
  let env = maybe (return EmptyGraph) (unitRectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ fromIntegral $ -10) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)


displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, each question will sound either as a pure tone or as a clipped version of that tone."
  elClass "div" "instructionsText" $ text "You can configure the exercise to have different levels of clipping. For example, at the -10 dB setting any clipped sound will be clipped 10 dB below its peak (extreme clipping) resulting in a markedly brighter, buzzier sound. At the -0.5 dB setting, the signal will be clipped just 0.5 dB below its peak, resulting in a more subtle difference from the unclipped alternative."


sourcesMap:: Map Int (String,SoundSourceConfigOption)
sourcesMap = fromList $ [(0,("300hz sine wave", Spec (Oscillator Sine (Hz 300)) (Just 2) ))]

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

{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.BoostOrCut (boostOrCutExercise) where

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
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.SpecEval

import InnerEar.Types.Data
import InnerEar.Widgets.UserMedia

type Config = Double -- representing amount of gain that is applied (or not)

configs :: [Config]
configs = [10,6,3,2,1,0.5,0.25,-0.25,-0.5,-1,-2,-3,-6,-10]

configMap:: Map Int (String,Config)
configMap = fromList $ zip [0::Int,1..] $ fmap (\x -> (show x ++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Boosted/Cut"
  show (Answer False) = "No Change"

answers = [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise you are either played a sound at an original/reference level, or a version of the sound that has been increased (boosted) or decreased (cut/attenuated) in level by a certain amount of dB of gain (positive dB for boost, negative dB for cut). When the amount of boost or cut is great, it is easier to tell the difference between the boosted/cut and reference/unchanged sound. The object of the exercise is to see how small you can make the difference while still being able to tell the two conditions apart."
  elClass "div" "instructionsText" $ text "Hint: before or after pressing Listen to hear the question, press 'Listen to Reference Sound' to hear the sound to which you are comparing the question sound."


renderAnswer::Map String Buffer -> Config -> (SourceNodeSpec,Maybe Time)-> Maybe Answer -> Synth ()
renderAnswer _ db (src, dur) (Just (Answer True)) = buildSynth $ do
  let env = maybe (return EmptyGraph) (rectEnv (Millis 1)) dur
  synthSource src >> gain (Db (-10+db)) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)
renderAnswer _ _ (src, dur) _ = buildSynth $ do
  let env = maybe (return EmptyGraph) (rectEnv (Millis 1)) dur
  synthSource src >> gain (Db $ -10) >> env >> destination
  maybeDelete (fmap (+Sec 0.2) dur)


displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayVerticalMultipleChoiceEvaluationGraph "" "" answers
--displayHistoricalEvaluationGraph "Historical Performance" "" answers
--displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

sourcesMap:: Map Int (String,Source)
sourcesMap = fromList $ [(0,("300hz sine wave", NodeSource (OscillatorNode $ Oscillator Sine 440 0) (Just 2))), (1,("Load a soundfile", NodeSource (BufferNode $ LoadedFile "boostOrCutExercise" (PlaybackParam 0 1 False)) Nothing))]

boostOrCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
boostOrCutExercise = multipleChoiceExercise
  1
  answers
  instructions
  (configWidget "boostOrCutExercise" sourcesMap 0 "Boost/Cut amount: " configMap) -- (dynRadioConfigWidget "fiveBandBoostCutExercise" sourcesMap 0  configMap)
  renderAnswer  -- c -> b->a->Sound
  BoostOrCut
  10
  displayEval
  generateQ

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

import InnerEar.Types.Data (Datum)
import InnerEar.Widgets.UserMedia

type Config = Double -- representing amount of gain that is applied (or not)

configs :: [Config]
configs = [10,6,3,2,1,0.5,0.25,-0.25,-0.5,-1,-2,-3,-6,-10]

configMap:: Map String Config
configMap = fromList $ fmap (\x -> (show x ++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Boosted/Cut"
  show (Answer False) = "No Change"

answers = [Answer False,Answer True]

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise you are either played a sound at an original/reference level, or a version of the sound that has been increased (boosted) or decreased (cut/attenuated) in level by a certain amount of dB of gain (positive dB for boost, negative dB for cut). When the amount of boost or cut is great, it is easier to tell the difference between the boosted/cut and reference/unchanged sound. The object of the exercise is to see how small you can make the difference while still being able to tell the two conditions apart."
  elClass "div" "instructionsText" $ text "Hint: before or after pressing Listen to hear the question, press 'Listen to Reference Sound' to hear the sound to which you are comparing the question sound."

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db s (Just (Answer True)) = GainSound (Sound s) (-10+db) -- 2.0 -- should be a source sound, attenuated by a standard amount (-10 dB) then boosted by dB
renderAnswer _ s (Just (Answer False)) = GainSound (Sound s) (-10)-- 2.0 -- should be just a source sound attenuated by standard amount (-10 dB)
renderAnswer db s Nothing = GainSound (Sound s) (-10)

boostOrCutConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
boostOrCutConfigWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the level of gain (boost) for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph' "Session Performance" "" answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

boostOrCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
boostOrCutExercise = multipleChoiceExercise
  1
  answers
  instructions
  (dynRadioConfigWidget "boostOrCutExercise" configMap)
  renderAnswer  -- c -> b->a->Sound
  BoostOrCut
  10
  boostOrCutConfigWidget
  displayEval
  generateQ

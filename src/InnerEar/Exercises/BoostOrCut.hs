{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.BoostOrCut (boostOrCutExercise) where

import Reflex
import Reflex.Dom
import Data.Map

import Reflex.Synth.Types
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Types.Data (Datum)

type Config = Double -- representing amount of gain that is applied (or not)

configs :: [Config]
configs = [10,6,3,2,1,0.5,0.25]

data Answer = Answer Bool deriving (Eq,Ord)

instance Show Answer where
  show (Answer True) = "Boosted"
  show (Answer False) = "Normal"

renderAnswer :: Config -> b -> Answer -> Sound
renderAnswer db _ (Answer True) = NoSound -- 2.0 -- should be a source sound, attenuated by a standard amount (-10 dB) then boosted by dB
renderAnswer db _ (Answer False) = NoSound -- 2.0 -- should be just a source sound attenuated by standard amount (-10 dB)

boostOrCutConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
boostOrCutConfigWidget i = radioConfigWidget msg configs i
  where msg = "Please choose the level of gain (boost) for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

boostOrCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
boostOrCutExercise = multipleChoiceExercise
  [Answer False,Answer True]
  trivialBWidget
  renderAnswer
  BoostOrCut
  10
  boostOrCutConfigWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")

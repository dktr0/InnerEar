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
import InnerEar.Widgets.UserMedia
import InnerEar.Types.Data (Datum)
import InnerEar.Widgets.UserMedia

type Config = Double -- representing amount of gain that is applied (or not)

configs :: [Config]
configs = [10,6,3,2,1,0.5,0.25]

data Answer = Answer Bool deriving (Eq,Ord)

instance Show Answer where
  show (Answer True) = "Boosted"
  show (Answer False) = "Normal"

renderAnswer :: Config -> Source -> Answer -> Sound
renderAnswer db s (Answer True) = GainSound (Sound s) (-10+db) -- 2.0 -- should be a source sound, attenuated by a standard amount (-10 dB) then boosted by dB
renderAnswer _ s (Answer False) = GainSound (Sound s) (-10)-- 2.0 -- should be just a source sound attenuated by standard amount (-10 dB)

boostOrCutConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
boostOrCutConfigWidget i = radioConfigWidget msg configs i
  where msg = "Please choose the level of gain (boost) for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]


boostOrCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
boostOrCutExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  (sourceWidget "boostOrCutExercise") -- m Dynamic t b
  renderAnswer  -- c -> b->a->Sound
  BoostOrCut
  10
  boostOrCutConfigWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")

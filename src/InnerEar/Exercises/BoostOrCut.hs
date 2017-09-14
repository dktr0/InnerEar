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
import InnerEar.Types.Data (Datum)
import InnerEar.Widgets.UserMedia

type Config = Double -- representing amount of gain that is applied (or not)

configs :: [Config]
configs = [10,6,3,2,1,0.5,0.25]

configMap:: Map String Config
configMap = fromList $ fmap (\x -> (show x ++" dB", x)) configs

data Answer = Answer Bool deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show (Answer True) = "Boosted"
  show (Answer False) = "Normal"

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db s (Just (Answer True)) = GainSound (Sound s) (-10+db) -- 2.0 -- should be a source sound, attenuated by a standard amount (-10 dB) then boosted by dB
renderAnswer _ s (Just (Answer False)) = GainSound (Sound s) (-10)-- 2.0 -- should be just a source sound attenuated by standard amount (-10 dB)
renderAnswer db s Nothing = GainSound (Sound s) (-10)

boostOrCutConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
boostOrCutConfigWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose the level of gain (boost) for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

boostOrCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
boostOrCutExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  (dynRadioConfigWidget "boostOrCutExercise" configMap)
  renderAnswer  -- c -> b->a->Sound
  BoostOrCut
  10
  boostOrCutConfigWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")

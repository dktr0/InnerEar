{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.HarmonicDistortion (harmonicDistortionExercise) where

import Reflex
import Reflex.Dom
import Data.Map

import Reflex.Synth.Types
import InnerEar.Exercises.MultipleChoice
import InnerEar.Types.ExerciseId
import InnerEar.Types.Exercise
import InnerEar.Types.Score
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data (Datum)

type Config = Double -- representing threshold of clipping, and inverse of post-clip normalization

configs :: [Config]
configs = [-12,-6,-3,-2,-1,-0.75,-0.5,-0.25,-0.125,-0.0625]

data Answer = Answer Bool deriving (Eq,Ord)

instance Show Answer where
  show (Answer True) = "Distorted"
  show (Answer False) = "Clean"

renderAnswer :: Config -> b -> Answer -> Sound
renderAnswer db _ (Answer True) = NoSound -- 2.0 -- should be a sine wave clipped and normalized by db, then attenuated a standard amount (-10 dB)
renderAnswer db _ (Answer False) = NoSound -- 2.0 -- should be a clean sine wave, just attenuated a standard amount (-10 dB)

harmonicDistortionConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
harmonicDistortionConfigWidget i = radioConfigWidget msg configs i
  where msg = "Please choose the level of clipping for this exercise:"

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion [Answer False,Answer True]

harmonicDistortionExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
harmonicDistortionExercise = multipleChoiceExercise
  1
  [Answer False,Answer True]
  renderAnswer
  HarmonicDistortion
  (-12)
  harmonicDistortionConfigWidget
  displayEval
  generateQ
  (Just "Please write a reflection here...")

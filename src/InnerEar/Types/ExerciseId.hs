{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.ExerciseId where

import Text.JSON
import Text.JSON.Generic

data ExerciseId =
  PrototypeExercise | 
  HarmonicsOne
  deriving (Show,Eq,Data,Typeable)

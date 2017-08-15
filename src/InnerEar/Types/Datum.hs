{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Datum where

import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Utility
import InnerEar.Types.ExerciseId

data Datum =
  SessionStart |
  SessionEnd |
  ExerciseStart ExerciseId |
  Score Int
  deriving (Show,Eq,Data,Typeable)

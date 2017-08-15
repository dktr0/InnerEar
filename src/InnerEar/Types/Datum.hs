{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Datum where

import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Utility
import InnerEar.Types.Exercise

data Datum =
  SessionStart |
  SessionEnd |
  ExerciseStart Exercise |
  Score Int
  deriving (Show,Eq,Data,Typeable)

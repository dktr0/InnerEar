{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Exercise where

import Text.JSON
import Text.JSON.Generic

data Exercise =
  TenBandPrototype |
  NotYetExistentExercise String String
  deriving (Show,Eq,Data,Typeable)

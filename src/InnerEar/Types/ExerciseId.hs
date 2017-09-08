{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.ExerciseId where

import Text.JSON
import Text.JSON.Generic

data ExerciseId =
  ThresholdOfSilence |
  HarmonicDistortion |
  BoostOrCut |
  FiveBandBoostCut |
  TenBandBoostCut |
  AddedWhiteNoise |
  RT60 |
  Compression |
  LeftRightCentre |
  HarmonicsOne
  deriving (Show,Read,Eq,Data,Typeable)

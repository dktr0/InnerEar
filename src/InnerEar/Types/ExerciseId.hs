{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.ExerciseId where

import Text.JSON
import Text.JSON.Generic

data ExerciseId =
  ThresholdOfSilence |
  HarmonicDistortion |
  GainBoost |
  FiveBandsBoostCut |
  AddedWhiteNoise |
  RT60 |
  CompressedUncompressed |
  LeftRightCentre |
  TenBandsBoostCut | 
  HarmonicsOne
  deriving (Show,Eq,Data,Typeable)

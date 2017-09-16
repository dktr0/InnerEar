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

showExerciseTitle :: ExerciseId -> String
showExerciseTitle ThresholdOfSilence = "Threshold of Silence"
showExerciseTitle HarmonicDistortion = "Harmonic Distortion"
showExerciseTitle BoostOrCut = "Boost or Cut"
showExerciseTitle FiveBandBoostCut = "Five Band Boost or Cut"
showExerciseTitle TenBandBoostCut = "Ten Band Boost or Cut"
showExerciseTitle AddedWhiteNoise = "Added White Noise"
showExerciseTitle RT60 = "RT60"
showExerciseTitle Compression = "Compression"
showExerciseTitle LeftRightCentre = "LeftRightCentre"
showExerciseTitle HarmonicsOne = "Harmonics One"
showExerciseTitle _ = "Error: unknown exercise"


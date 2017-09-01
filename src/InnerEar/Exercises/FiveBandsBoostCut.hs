{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.FiveBandsBoostCut (fiveBandsBoostCutExercise) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import Data.Map
import Data.List (elemIndex,findIndices)
import System.Random
import Text.JSON
import Text.JSON.Generic

import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data
import InnerEar.Types.Score
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.Frequency
import InnerEar.Exercises.MultipleChoice

type Config = Double

configs :: [Double]
configs = [10,6,3,2,1,-1,-2,-3,-6,-10]

type Answer = Frequency

frequencies :: [Answer]
frequencies = [F 155 "Bass (155 Hz)",F 1125 "Low Mids (1125 Hz)",F 3000 "High Mids (3 kHz)",
  F 5000 "Presence (5 kHz)",F 13000 "Brilliance (13 kHz)"]

sound :: Config -> Source -> Answer -> Sound
sound db f = FilteredSound source filter -- needs to be boost or cut by specified dB
  where source = NodeSource (BufferNode $ File "pinknoise.wav") 2.0
        filter = Filter Peaking (freqAsDouble f) 1.4 16.0 -- and bandwidth should be wider

configWidget :: MonadWidget t m => Config -> m (Event t Config)
configWidget i = radioConfigWidget msg configs i
  where msg = "Please choose how many decibels (dB) of boost or cut may (or may not) be applied during the exercise."

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval scoreMap = return ()

generateQuestion :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQuestion _ _ = randomMultipleChoiceQuestion frequencies

fiveBandsBoostCutExercise :: MonadWidget t m => Exercise t m WhatBandsAreAllowed [Frequency] Frequency (Map Frequency Score)
fiveBandsBoostCutExercise = multipleChoiceExercise
  answers
  sound
  FiveBandsBoostCut
  (configs!!0)
  configWidget
  displayEval
  generateQuestion
  (Just "Please write some brief text reflecting on your experience:")

{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.FiveBandBoostCut (fiveBandBoostCutExercise) where

import Reflex
import Reflex.Dom
import Data.Map
import Data.List (elemIndex,findIndices)
import System.Random
import Text.JSON
import Text.JSON.Generic

import InnerEar.Widgets.Config
import InnerEar.Widgets.SpecEval
import InnerEar.Types.Data
import InnerEar.Types.Score
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Types.Frequency
import InnerEar.Exercises.MultipleChoice
import InnerEar.Widgets.UserMedia

type Config = Double

configs :: [Double]
configs = [10,6,3,2,1,-1,-2,-3,-6,-10]

configMap::Map String Config
configMap = fromList $ fmap (\x-> (show x ++ " dB",x)) configs

type Answer = Frequency

answers :: [Answer]
answers = [F 155 "Bass (155 Hz)",F 1125 "Low Mids (1125 Hz)",F 3000 "High Mids (3 kHz)",
  F 5000 "Presence (5 kHz)",F 13000 "Brilliance (13 kHz)"]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer db s f = case f of
  (Just freq) -> GainSound (FilteredSound s $ Filter Peaking (freqAsDouble freq) 1.4 db) (-10)
  Nothing -> GainSound (Sound s) (-10)

fiveBandConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
fiveBandConfigWidget i = radioConfigWidget "" msg configs i
  where msg = "Please choose how many decibels (dB) of boost or cut may (or may not) be applied during the exercise."

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displaySpectrumEvaluation (constDyn "Session Performance")

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ _ _ = randomMultipleChoiceQuestion answers

fiveBandBoostCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
fiveBandBoostCutExercise = multipleChoiceExercise
  3
  answers
  (return ())
  (dynRadioConfigWidget "fiveBandBoostCutExercise" configMap)
  renderAnswer
  FiveBandBoostCut
  (configs!!0)
  fiveBandConfigWidget
  displayEval
  generateQ

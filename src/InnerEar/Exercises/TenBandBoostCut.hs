{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.TenBandBoostCut (tenBandBoostCutExercise) where

import Reflex
import Reflex.Dom
import Data.Map
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
import InnerEar.Widgets.Config
import InnerEar.Widgets.UserMedia

data Config = AllBands | HighBands | MidBands | Mid8Bands | LowBands deriving (Show,Eq,Data,Typeable)

configMap::Map String Config
configMap = fromList $ zip ["Entire spectrum", "High bands", "Mid bands", "Mid 8 bands", "Low bands"] [AllBands, HighBands, MidBands, Mid8Bands, LowBands]

type Answer = Frequency

answers :: [Answer]
answers = [
  F 31 "31", F 63 "63", F 125 "125", F 250 "250", F 500 "500",
  F 1000 "1k", F 2000 "2k", F 4000 "4k", F 8000 "8k", F 16000 "16k"]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer _ s f = case f of
  (Just freq) -> GainSound (FilteredSound s $ Filter Peaking (freqAsDouble freq) 1.4 16) (-10)
  Nothing -> GainSound (Sound s) (-10)

tenBandConfigWidget :: MonadWidget t m => Config -> m (Event t Config)
tenBandConfigWidget i = radioConfigWidget "" msg possibilities i
  where msg = "Please choose the spectrum range you would like to practice:"
        possibilities = [AllBands,HighBands,MidBands,Mid8Bands,LowBands]

convertBands :: Config -> [Answer]
convertBands AllBands = answers
convertBands HighBands = drop 5 answers
convertBands MidBands = take 5 $ drop 3 $ answers
convertBands Mid8Bands = take 8 $ drop 1 $ answers
convertBands LowBands = take 5 answers

generateQ :: Config -> [Datum Config [Answer] Answer (Map Answer Score)] -> IO ([Answer],Answer)
generateQ config _ = randomMultipleChoiceQuestion (convertBands config)

tenBandBoostCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
tenBandBoostCutExercise = multipleChoiceExercise
  3
  answers
  (dynRadioConfigWidget "tenBandsExercise" configMap)
  renderAnswer
  TenBandBoostCut
  AllBands
  tenBandConfigWidget
  (displayCurrentSpectrumEvaluation "Session Performance")
  generateQ
  (Just "Please write some brief text reflecting on your experience:")

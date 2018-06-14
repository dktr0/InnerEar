{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Exercises.TenBandBoostCut (tenBandBoostCutExercise) where

import Control.Monad (liftM)
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
import InnerEar.Widgets.Utility (radioWidget, safeDropdown)
import InnerEar.Widgets.AnswerButton

data FrequencyBand = AllBands | HighBands  | Mid8Bands | MidBands | LowBands deriving (Eq,Data,Typeable, Read)

instance Show FrequencyBand where
  show (AllBands) = "Entire spectrum"
  show (HighBands) = "High Bands"
  show (MidBands) = "Mid Bands"
  show (Mid8Bands) = "Mid 8 Bands"
  show (LowBands) = "Low Bands"


-- necessary so things are displayed in the config dropdown in the right order
instance Ord FrequencyBand where
  compare AllBands AllBands = EQ
  compare AllBands _ = LT
  compare HighBands AllBands = GT
  compare HighBands HighBands = EQ
  compare HighBands MidBands = LT
  compare HighBands Mid8Bands = LT
  compare HighBands LowBands = LT
  compare MidBands AllBands = GT
  compare MidBands HighBands = GT
  compare MidBands MidBands = EQ
  compare MidBands Mid8Bands = LT
  compare MidBands LowBands = LT
  compare Mid8Bands AllBands =GT
  compare Mid8Bands HighBands = GT
  compare Mid8Bands MidBands = GT
  compare Mid8Bands Mid8Bands = EQ
  compare Mid8Bands LowBands = LT
  compare LowBands LowBands = EQ
  compare LowBands _ = GT


frequencyBands::[FrequencyBand]
frequencyBands = [AllBands, HighBands, Mid8Bands, MidBands, LowBands]

boostAmounts::[Double]
boostAmounts = reverse [-10,-6,-3,-2,-1,1,2,3,6,10]

type Config = (FrequencyBand, Double) -- FrequencyBand and Boost/Cut amount

newtype Answer = Answer { frequency :: Frequency } deriving (Eq,Ord,Data,Typeable)

instance Show Answer where
  show a = freqAsString $frequency a

instance Buttonable Answer where
  makeButton = showAnswerButton

answers :: [Answer]
answers = [
  Answer $ F 31 "31",Answer $ F 63 "63", Answer $ F 125 "125", Answer $ F 250 "250", Answer $ F 500 "500",
  Answer $ F 1000 "1k", Answer $ F 2000 "2k", Answer $ F 4000 "4k", Answer $ F 8000 "8k", Answer $ F 16000 "16k"]

renderAnswer :: Config -> Source -> Maybe Answer -> Sound
renderAnswer (_,boost) s f = case f of
  (Just freq) -> GainSound (FilteredSound s $ Filter Peaking (freqAsDouble (frequency freq)) 1.4 boost) (-10)
  Nothing -> GainSound (Sound s) (-10)

convertBands :: FrequencyBand -> [Answer]
convertBands AllBands = answers
convertBands HighBands = drop 5 answers
convertBands MidBands = take 5 $ drop 3 $ answers
convertBands Mid8Bands = take 8 $ drop 1 $ answers
convertBands LowBands = take 5 answers

instructions :: MonadWidget t m => m ()
instructions = el "div" $ do
  elClass "div" "instructionsText" $ text "In this exercise, a filter is applied to a specific region of the spectrum, either boosting or cutting the energy in that part of the spectrum by a specified amount. Your task is to identify which part of the spectrum has been boosted or cut. Challenge yourself and explore additional possibilities by trying cuts (instead of boosts) to the spectrum, and by trying more subtle boosts/cuts (dB values progressively closer to 0)."

generateQ :: Config -> [ExerciseDatum] -> IO ([Answer],Answer)
generateQ config _ = randomMultipleChoiceQuestion (convertBands $ fst config)

displayEval :: MonadWidget t m => Dynamic t (Map Answer Score) -> m ()
displayEval = displayMultipleChoiceEvaluationGraph ("scoreBarWrapperTenBars","svgBarContainerTenBars","svgFaintedLineTenBars","xLabelTenBars") "Session Performance" "" answers

sourcesMap:: Map Int (String,Source)
sourcesMap = fromList $ zip [0::Int,1..] [("Pink noise",NodeSource (BufferNode $ File "pinknoise.wav") (Just 2)), ("White noise", NodeSource (BufferNode $ File "whitenoise.wav") (Just 2)), ("Load a soundfile", NodeSource (BufferNode $ LoadedFile "tenBandBoostCutExercise" (PlaybackParam 0 1 False)) Nothing)]

-- temporary until config widget is changed to take a list/map of config parameters that can be changed
tenBandsConfigWidget::MonadWidget t m => Config -> m (Dynamic t Config,  Dynamic t Source,  Event t (Maybe a)) -- dyn config, source, and event maybe answer for playing reference sound (config widget
tenBandsConfigWidget c =  elClass "div" "configWidget" $ do
  config <- elClass "div" "tenBandsConfigWidget" $ do
    text "Spectrum Range: "
    (bands,_) <- safeDropdown (fst c) (fromList $ fmap (\x->(x,show x)) frequencyBands) (constDyn empty) never
    let boostMap = fromList $ zip [(0::Int),1..] boostAmounts
    let invertedBoostMap = fromList $ zip boostAmounts [(0::Int),1..]
    let initialVal = maybe 0 id $ Data.Map.lookup  (snd c) invertedBoostMap
    text "Boost/Cut amount: "
    boost <- liftM  _dropdown_value $ dropdown initialVal (constDyn $ fmap show boostMap) def
    boost' <- mapDyn (maybe 10 id . (flip Data.Map.lookup) boostMap) boost
    combineDyn (, ) bands  boost'
  (source,playReference) <- sourceWidget "tenBandBoostCutExercise" sourcesMap 0
  return (config, source, Nothing <$ playReference)


tenBandBoostCutExercise :: MonadWidget t m => Exercise t m Config [Answer] Answer (Map Answer Score)
tenBandBoostCutExercise = multipleChoiceExercise
  3
  answers
  instructions
  tenBandsConfigWidget
  renderAnswer
  TenBandBoostCut
  (AllBands, 10)
  displayEval
  generateQ

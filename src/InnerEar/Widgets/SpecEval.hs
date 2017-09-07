module InnerEar.Widgets.SpecEval where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad

import InnerEar.Types.Frequency
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars
import InnerEar.Types.Score
import InnerEar.Widgets.Labels


displaySpectrumEvaluation :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Frequency Score) -> m ()
displaySpectrumEvaluation graphLabel score = elClass "div" "specEvalWrapper" $ do
  dynGraphLabel (constDyn "graphLabel") graphLabel
  maybeScore<- mapDyn (mapKeys (freqAsString) . fmap (\v-> case v of (Score 0 0 0)-> Nothing;otherwise->Just v)) score
  listWithKey maybeScore scoreBar
  return ()

{-
displayMultipleChoiceEvaluation :: (MonadWidget t m, Show a, Ord a)
  => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluation graphLabel xLabel possibilities scoreMap = ...
-}

evalGraphFrame :: MonadWidget t m =>  String -> String -> m ()
evalGraphFrame xMainLabel graphLabel = elClass "div" "evalGraphFrameRow" $ do
  faintedYaxis "faintedYaxis"
  faintedXaxis "faintedXaxis"
  hzMainLabel "hzMainLabel" xMainLabel
  countMainLabel "countMainLabel" "#"
  percentageMainLabel "percentageMainLabel" "%"
  elClass "div" "graphLabel" $ text graphLabel
  return ()


displayCurrentSpectrumEvaluation :: MonadWidget t m => String -> Dynamic t (Map Frequency Score) -> m ()
displayCurrentSpectrumEvaluation graphLabel score = elClass "div" "specEvalWrapper" $ do
  evalGraphFrame "Hz" graphLabel
  let labels = ["31","63","125","250","500","1","2","4","8","16"]
  let frequencies = zipWith F [31::Double,63,125,250,500,1000,2000,4000,8000,16000] labels -- [Frequency]
  let band0Hz = (!!0) labels -- String
  let band1Hz = (!!1) labels
  let band2Hz = (!!2) labels
  let band3Hz = (!!3) labels
  let band4Hz = (!!4) labels
  let band5Hz = (!!5) labels
  let band6Hz = (!!6) labels
  let band7Hz = (!!7) labels
  let band8Hz = (!!8) labels
  let band9Hz = (!!9) labels

  band0Score <- mapDyn (Data.Map.lookup (frequencies!!0)) score --  Dynamic t (Score) ?
  band1Score <- mapDyn (Data.Map.lookup (frequencies!!1)) score
  band2Score <- mapDyn (Data.Map.lookup (frequencies!!2)) score
  band3Score <- mapDyn (Data.Map.lookup (frequencies!!3)) score
  band4Score <- mapDyn (Data.Map.lookup (frequencies!!4)) score
  band5Score <- mapDyn (Data.Map.lookup (frequencies!!5)) score
  band6Score <- mapDyn (Data.Map.lookup (frequencies!!6)) score
  band7Score <- mapDyn (Data.Map.lookup (frequencies!!7)) score
  band8Score <- mapDyn (Data.Map.lookup (frequencies!!8)) score
  band9Score <- mapDyn (Data.Map.lookup (frequencies!!9)) score

  band0ScoreBar <- scoreBar band0Hz band0Score-- m ()
  band1ScoreBar <- scoreBar band1Hz band1Score
  band2ScoreBar <- scoreBar band2Hz band2Score
  band3ScoreBar <- scoreBar band3Hz band3Score
  band4ScoreBar <- scoreBar band4Hz band4Score
  band5ScoreBar <- scoreBar band5Hz band5Score
  band6ScoreBar <- scoreBar band6Hz band6Score
  band7ScoreBar <- scoreBar band7Hz band7Score
  band8ScoreBar <- scoreBar band8Hz band8Score
  band9ScoreBar <- scoreBar band9Hz band9Score
  return ()

displaySpectrumEvaluationFiveBand :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Frequency Score) -> m ()
displaySpectrumEvaluationFiveBand graphLabel score = elClass "div" "specEvalWrapper" $ do

    faintedYaxis "faintedYaxisFiveBand"
    hzMainLabel "hzMainLabelFiveBand" "Hz"
    countMainLabel "countMainLabelFiveBand" "#"

    let labels = ["Bass (155 Hz)", "Low Mids (1125 Hz)", "High Mids (3 kHz)", "Presence (5 kHz)", "Brilliance (13 kHz)"]
    let frequencies = zipWith F [155::Double, 1125, 3000, 5000, 13000] labels -- [Frequency]
    let band0Hz = (!!0) labels -- String
    let band1Hz = (!!1) labels
    let band2Hz = (!!2) labels
    let band3Hz = (!!3) labels
    let band4Hz = (!!4) labels

    band0Score <- mapDyn (Data.Map.lookup (frequencies!!0)) score --  Dynamic t (Score) ?
    band1Score <- mapDyn (Data.Map.lookup (frequencies!!1)) score
    band2Score <- mapDyn (Data.Map.lookup (frequencies!!2)) score
    band3Score <- mapDyn (Data.Map.lookup (frequencies!!3)) score
    band4Score <- mapDyn (Data.Map.lookup (frequencies!!4)) score

    band0ScoreBar <- scoreBar' band0Hz band0Score-- m ()
    band1ScoreBar <- scoreBar' band1Hz band1Score
    band2ScoreBar <- scoreBar' band2Hz band2Score
    band3ScoreBar <- scoreBar' band3Hz band3Score
    band4ScoreBar <- scoreBar' band4Hz band4Score
    percentageMainLabel "percentageMainLabelFiveBand" "%"
    faintedXaxis "faintedXaxisFive"
    dynGraphLabel (constDyn "graphLabelFiveBand") graphLabel

    return ()


displayMultipleChoiceEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph graphLabel xLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
  elClass "div" "graphLabel" $ text graphLabel
  elClass "div"  "xLabel" $ text xLabel
--  questionScore <- mapDyn (mapKeys a) score
  let questionLabel1 = show $ (!!0) possibilities
  let questionLabel2 = show $ (!!1) possibilities

  questionScore1 <- mapDyn (Data.Map.lookup (possibilities!!0)) scoreMap
  questionScore2 <- mapDyn (Data.Map.lookup (possibilities!!1)) scoreMap

  questionScoreBar1 <- scoreBar questionLabel1 questionScore1
  questionScoreBar2 <- scoreBar questionLabel2 questionScore2

  return ()

displayMultipleChoiceEvaluationGraph' :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph' graphLabel xLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      listWithKey scoreMap' f
      faintedXaxis "faintedXaxis"
      faintedYaxis "faintedYaxis"
      percentageMainLabel "percentageMainLabel" "%"
      elClass "div"  "xLabel" $ text xLabel
      countMainLabel "countMainLabel" "#"
      elClass "div" "graphLabel" $ text graphLabel
      return ()
      where f k d = scoreBar (show k) d

--Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
--  scoreBar :: MonadWidget t m => String -> Dynamic t (Maybe Score) ->  m ()

{-
displaySpectrumEvaluationGraphs :: MonadWidget t m => m ()
displaySpectrumEvaluationGraphs = do
  --  let currentM = constDyn (Data.Map.fromList [((F 31 "31"), (Score 1 0 9)), ((F 63 "63"), (Score 2 0 8)), ((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1)), ((F 16000 "16"), (Score 10 0 0))])
  --  let currentM = constDyn (Data.Map.fromList [((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1)), ((F 16000 "16"), (Score 10 0 0))])
  --  let currentM = constDyn (Data.Map.fromList [((F 63 "63"), (Score 2 0 8)),((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1))])
  --  let currentM = constDyn (Data.Map.fromList [((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2))])
  --  let currentM = constDyn (Data.Map.fromList [((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3))])
  --  let currentM = constDyn (Data.Map.fromList [((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4))])
  --  let currentM = constDyn (Data.Map.fromList [((F 500 "500"), (Score 5 0 5))])
  --  displayCurrentSpectrumEvaluation (constDyn "Current Performance") currentM
-}

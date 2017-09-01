module InnerEar.Widgets.SpecEval where

import Reflex
import Reflex.Dom
import Data.Map as M
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

displayCurrentSpectrumEvaluation :: MonadWidget t m => Dynamic t String -> Dynamic t (Map Frequency Score) -> m ()
displayCurrentSpectrumEvaluation graphLabel score = elClass "div" "specEvalWrapper" $ do

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

  band0Score <- mapDyn (M.lookup (frequencies!!0)) score --  Dynamic t (Score) ?
  band1Score <- mapDyn (M.lookup (frequencies!!1)) score
  band2Score <- mapDyn (M.lookup (frequencies!!2)) score
  band3Score <- mapDyn (M.lookup (frequencies!!3)) score
  band4Score <- mapDyn (M.lookup (frequencies!!4)) score
  band5Score <- mapDyn (M.lookup (frequencies!!5)) score
  band6Score <- mapDyn (M.lookup (frequencies!!6)) score
  band7Score <- mapDyn (M.lookup (frequencies!!7)) score
  band8Score <- mapDyn (M.lookup (frequencies!!8)) score
  band9Score <- mapDyn (M.lookup (frequencies!!9)) score

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
  faintedXaxis
  faintedYaxis
  percentageMainLabel
  hzMainLabel
  countMainLabel
  dynGraphLabel (constDyn "graphLabel") graphLabel

  return ()


displayMultipleChoiceEvaluationGraphs :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraphs graphLabel xLabel possibilities scoreMap = do
  elClass "div" "graphLabel" $ text graphLabel
  elClass "div"  "xLabel" $ text xLabel
--  questionScore <- mapDyn (mapKeys a) score
  let questionLabel1 = show $ (!!0) possibilities
  let questionLabel2 = show $ (!!1) possibilities

  questionScore1 <- mapDyn (M.lookup (possibilities!!0)) scoreMap
  questionScore2 <- mapDyn (M.lookup (possibilities!!1)) scoreMap

  questionScoreBar1 <- scoreBar questionLabel1 questionScore1
  questionScoreBar2 <- scoreBar questionLabel2 questionScore2

  return ()

displayMultipleChoiceEvaluationGraphs' :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraphs' graphLabel xLabel possibilities scoreMap = do
      elClass "div" "graphLabel" $ text graphLabel
      elClass "div"  "xLabel" $ text xLabel
      let possibilities' = show possibilities
      score <- mapDyn (mapKeys (\_ -> possibilities') . fmap (\v -> case v of (Score 0 0 0) -> Nothing; otherwise -> Just v)) scoreMap -- Dynamic t (Map k v)
      listWithKey score scoreBar --Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
      return ()

--  scoreBar :: MonadWidget t m => Dynamic t (Maybe Score) -> String ->  m ()

{-
displaySpectrumEvaluationGraphs :: MonadWidget t m => m ()
displaySpectrumEvaluationGraphs = do
  --  let currentM = constDyn (M.fromList [((F 31 "31"), (Score 1 0 9)), ((F 63 "63"), (Score 2 0 8)), ((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1)), ((F 16000 "16"), (Score 10 0 0))])
  --  let currentM = constDyn (M.fromList [((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1)), ((F 16000 "16"), (Score 10 0 0))])
  --  let currentM = constDyn (M.fromList [((F 63 "63"), (Score 2 0 8)),((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2)), ((F 8000 "8"), (Score 9 0 1))])
  --  let currentM = constDyn (M.fromList [((F 125 "125"), (Score 3 0 7)), ((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3)), ((F 4000 "4"), (Score 8 0 2))])
  --  let currentM = constDyn (M.fromList [((F 250 "250"), (Score 4 0 6)), ((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4)), ((F 2000 "2"), (Score 7 0 3))])
  --  let currentM = constDyn (M.fromList [((F 500 "500"), (Score 5 0 5)), ((F 1000 "1"), (Score 6 0 4))])
  --  let currentM = constDyn (M.fromList [((F 500 "500"), (Score 5 0 5))])
  --  displayCurrentSpectrumEvaluation (constDyn "Current Performance") currentM
-}

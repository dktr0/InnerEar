{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Widgets.SpecEval where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad
import qualified Data.Text as T
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)

import InnerEar.Types.Frequency
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Bars
import InnerEar.Types.Score
import InnerEar.Widgets.Labels


evalGraphFrame :: MonadWidget t m =>  String -> String -> m ()
evalGraphFrame xMainLabel graphLabel = do
  faintedYaxis "faintedYaxis"
  hzMainLabel "hzMainLabel" xMainLabel
  countMainLabel "countMainLabel" "#"
  percentageMainLabel "percentageMainLabel" "%"
  elClass "div" "graphLabel" $ text graphLabel
  return ()


--answerCases :: String -> String?
answerCases a = case (show a) of "Answer {frequency = Bass (155 Hz)}" -> "Bass (155 Hz)"
                                 "Answer {frequency = Low Mids (1125 Hz)}" -> "Low Mids (1125 Hz)"
                                 "Answer {frequency = High Mids (3 kHz)}" -> "High Mids (3 kHz)"
                                 "Answer {frequency = Presence (5 kHz)}" -> "Presence (5 kHz)"
                                 "Answer {frequency = Brilliance (13 kHz)}" -> "Brilliance (13 kHz)"
                                 "Answer {frequency = 31}" ->  "31Hz"
                                 "Answer {frequency = 63}" -> "63Hz"
                                 "Answer {frequency = 125}" -> "125Hz"
                                 "Answer {frequency = 250}" -> "250Hz"
                                 "Answer {frequency = 500}" -> "500Hz"
                                 "Answer {frequency = 1k}" ->  "1k"
                                 "Answer {frequency = 2k}" ->  "2k"
                                 "Answer {frequency = 4k}" ->  "4k"
                                 "Answer {frequency = 8k}" ->  "8k"
                                 "Answer {frequency = 16k}" -> "16k"
                                 _ -> show a

displayMultipleChoiceEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => (String, String, String, String) -> String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph (scoreBarWrapperClass, svgBarContainerClass, svgFaintedLineClass, xLabelClass) graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      evalGraphFrame qLabel graphLabel
      listWithKey scoreMap' f
      return ()
      where f k d = scoreBar (scoreBarWrapperClass, svgBarContainerClass, svgFaintedLineClass, xLabelClass) (answerCases k) d


displayHistoricalEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayHistoricalEvaluationGraph graphLabel qLabel possibilities currentScoreMap = elClass "div" "specEvalWrapper" $ do
     currentScoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) currentScoreMap
     currentScoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) currentScoreList
     evalGraphFrame qLabel graphLabel
     listWithKey currentScoreMap' f -- Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
     return ()
       where
         f k d = scoreBarH ("scoreBarWrapperHist","svgBarContainerCurrent", "svgBarContainerHist", "svgFaintedLineCurrent","svgFaintedLineHist", "xLabel") (show k) d


{- x graphLabel qLabel possibilities scoreMap = do
 listOfScores <- mapDyn ...
 let qLabel' = Line 400 300 200 200
 let possibilities = Rect 400 300 200 100
 scores <- mapDyn (\x -> Line x 300 150 100) listOfScores
 allDrawingInstructions <- mapDyn (\x -> qLabel':possibilities:x) scores
 dynSvg allDrawingInstructions -}

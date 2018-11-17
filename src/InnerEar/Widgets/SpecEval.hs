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

displayMultipleChoiceEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => (String, String, String, String) -> String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph (scoreBarWrapperClass, svgBarContainerClass, svgFaintedLineClass, xLabelClass) graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      -- evalGraphFrame qLabel graphLabel
      listWithKey scoreMap' f
      return ()
      where f k d = scoreBar (scoreBarWrapperClass, svgBarContainerClass, svgFaintedLineClass, xLabelClass) (show k) d


--a historical evaluation graph
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

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

evalGraphFrame :: MonadWidget t m =>  String -> String -> m ()
evalGraphFrame xMainLabel graphLabel = do
  faintedYaxis "faintedYaxis"
  hzMainLabel "hzMainLabel" xMainLabel
  countMainLabel "countMainLabel" "#"
  percentageMainLabel "percentageMainLabel" "%"
  elClass "div" "graphLabel" $ text graphLabel
  return ()

displayMultipleChoiceEvaluationGraph' :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph' graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      evalGraphFrame qLabel graphLabel
      listWithKey scoreMap' f
      return ()
      where f k d = scoreBar ("scoreBarWrapper","svgBarContainer","svgFaintedLine", "xLabel") (show k) d

displayMultipleChoiceEvaluationGraph'' :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph'' graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      evalGraphFrame qLabel graphLabel
      listWithKey scoreMap' f
      return ()
      where f k d = scoreBar ("scoreBarWrapperFiveBand","svgBarContainerFiveBand","svgFaintedLineFiveBand", "xLabelFiveBand") (show k) d

displayMultipleChoiceEvaluationGraph''' :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayMultipleChoiceEvaluationGraph''' graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapper" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      evalGraphFrame qLabel graphLabel
      listWithKey scoreMap' f
      return ()
      where f k d = scoreBar ("scoreBarWrapperTenBand","svgBarContainerTenBand","svgFaintedLineTenBand","xLabelTenBand") (show k) d
        --elClass "div" "graphSpace"



{- x graphLabel qLabel possibilities scoreMap = do
 listOfScores <- mapDyn ...
 let qLabel' = Line 400 300 200 200
 let possibilities = Rect 400 300 200 100
 scores <- mapDyn (\x -> Line x 300 150 100) listOfScores
 allDrawingInstructions <- mapDyn (\x -> qLabel':possibilities:x) scores
 dynSvg allDrawingInstructions -}

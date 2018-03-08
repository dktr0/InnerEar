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

evalGraphFrame' :: MonadWidget t m =>  String ->  String -> String -> m ()
evalGraphFrame' xMainLabel countMainLabelClass percentageMainLabelClass = do
    faintedYaxis "faintedYaxis"
    hzMainLabel "hzMainLabel" xMainLabel
    countMainLabel countMainLabelClass "#"
    percentageMainLabel percentageMainLabelClass "%"
    --elClass "div" "graphLabel" $ text graphLabel
    return ()

graphDropdown :: MonadWidget t m => m ()
graphDropdown = el "div" $ do
  ddVal <- el "div" $ do
    text "Graph selection: "
    let typesOfGraph = Data.Map.fromList [(1 :: Int, "Session Performance"), (2, "Historical Performance")]
    dd <- dropdown 1 (constDyn typesOfGraph) def
    return $ _dropdown_value dd
  return ()

{--switchGraph :: Bool -> String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
switchGraph bool graphLabel qLabel possibilities currentScoreMap = do
  --bool <- mapDyn (maybe False (const True)) selection
  let current = displayHistoricalEvaluationGraph  graphLabel qLabel possibilities currentScoreMap
  flippableDyn (return ()) current bool
  let historical = displayHistoricalEvaluationGraph  graphLabel qLabel possibilities currentScoreMap
  flippableDyn historical (return ()) bool
--}

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

displayVerticalMultipleChoiceEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayVerticalMultipleChoiceEvaluationGraph graphLabel qLabel possibilities scoreMap = elClass "div" "specEvalWrapperV" $ do
      scoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) scoreMap -- m (Dynamic t [Maybe Score])
      scoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) scoreList -- (Dynamic t (Map a (Maybe Score)))
      evalGraphFrame' qLabel "countMainLabelV" "percentageMainLabelV"
      listWithKey scoreMap' f
      faintedYaxis "faintedYaxisV"
      return ()
      where f k d = scoreBarV ("scoreBarWrapperVertical","svgBarContainerVertical","svgFaintedLineVertical","xLabelVertical") (show k) d

displayHistoricalEvaluationGraph :: (MonadWidget t m, Show a, Ord a) => String -> String -> [a] -> Dynamic t (Map a Score) -> m ()
displayHistoricalEvaluationGraph graphLabel qLabel possibilities currentScoreMap = elClass "div" "specEvalWrapper" $ do
     currentScoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) currentScoreMap
     currentScoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) currentScoreList
     graphDropdown
     evalGraphFrame qLabel graphLabel
     listWithKey currentScoreMap' f -- Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
     return ()
       where
         f k d = scoreBarH ("scoreBarWrapperHist","svgBarContainerCurrent", "svgBarContainerHist", "svgFaintedLineCurrent","svgFaintedLineHist", "xLabel") (show k) d

displayHistoricalEvaluationGraph2 :: (MonadWidget t m, Show a, Ord a) => String -> [a] -> Dynamic t (Map a Score) -> m ()
displayHistoricalEvaluationGraph2 qLabel possibilities currentScoreMap = elClass "div" "specEvalWrapper" $ do
  --  graphDropdown >> evalGraphFrame' qLabel
    currentScoreList <- mapDyn (\x -> fmap (\y -> Data.Map.lookup y x) possibilities) currentScoreMap
    currentScoreMap' <- mapDyn (\x -> fromList $ zip possibilities x) currentScoreList

    listWithKey currentScoreMap' f -- Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
    return ()
      where
      f k d = scoreBarInvertedH ("scoreBarWrapperHistMirror","svgBarContainerCurrentMirror", "svgBarContainerHistMirror", "svgFaintedLineCurrentMirror","svgFaintedLineHistMirror", "xLabelMirror") (show k) d



{- x graphLabel qLabel possibilities scoreMap = do
 listOfScores <- mapDyn ...
 let qLabel' = Line 400 300 200 200
 let possibilities = Rect 400 300 200 100
 scores <- mapDyn (\x -> Line x 300 150 100) listOfScores
 allDrawingInstructions <- mapDyn (\x -> qLabel':possibilities:x) scores
 dynSvg allDrawingInstructions -}

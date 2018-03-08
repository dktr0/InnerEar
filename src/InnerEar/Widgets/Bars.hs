module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad
import Data.Monoid
import Data.Maybe (isJust)
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Labels
import InnerEar.Types.Score

replaceEach ::  String -> String
replaceEach [] = []
replaceEach (x:xs)
   |x == '(' = replaceEach xs
   |x == ')' = replaceEach xs
   |otherwise = x:replaceEach xs

listToString :: [(Double,Double)] -> String
listToString [] = []
listToString (x:xs) = concat [replaceEach $ show x, " ", listToString xs]

listToString' :: [Double] -> String
listToString' [] = []
listToString' (x:y:zs) = concat [ show x, ",", show y, " ",  listToString' zs]

shapeLine ::  MonadWidget t m => [Double] -> m ()
shapeLine listOfPoints = do
    svgClass "svg" "shapeContainer" $ do
      let listOfPoints' = listToString' listOfPoints
      let c = constDyn (singleton "style" "fill:none;stroke:black;stroke-width:3")
      let points = constDyn (singleton "points" listOfPoints')
      attrs <- mconcatDyn [c, points]
      svgDynAttr "polyline" attrs $ return ()

      --instructions dynamic t c

dynBarCSS' :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> Dynamic t String -> m ()
dynBarCSS' percent barWidth c =  do
    class' <- mapDyn (singleton "class") c
    --let class' = constDyn (singleton "class" "svgBarContainer")
    svgHeight <- mapDyn (* 150) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    let rotate' = constDyn (singleton "style" "transform:rotate(-180deg)")
    attrs <- mconcatDyn [class', svgHeight', rotate']
    svgDynAttr "svg" attrs $ return ()


dynBarCSSV :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> Dynamic t String -> m ()
dynBarCSSV percent barWidth c =  do
    class' <- mapDyn (singleton "class") c
   --let class' = constDyn (singleton "class" "svgBarContainer")
    svgHeight <- mapDyn (* 150) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    let rotate' = constDyn (singleton "style" "transform:rotate(90deg)")
    attrs <- mconcatDyn [class', svgHeight', rotate']
    svgDynAttr "svg" attrs $ return ()

--inverted bars
dynBarCSSInv :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> Dynamic t String -> m ()
dynBarCSSInv percent barWidth c =  do
    class' <- mapDyn (singleton "class") c
    --let class' = constDyn (singleton "class" "svgBarContainer")
    svgHeight <- mapDyn (* 150) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    --let svgPos = constDyn 70
    --let svgPos' = constDyn (singleton "top" . show) svgPos
    let rotate' = constDyn (singleton "style" "transform:rotate(-180deg)")
    attrs <- mconcatDyn [class', svgHeight', rotate']
    svgDynAttr "svg" attrs $ return ()

-- A small fainted line to use in performance graphs
faintedLineCSS :: MonadWidget t m => String -> m ()
faintedLineCSS s = svgClass "svg" s $ return ()

--A fainted Y axis
faintedYaxis :: MonadWidget t m => String -> m ()
faintedYaxis s =  svgClass "svg" s $ return ()

-- A fainted X axis
faintedXaxis :: MonadWidget t m => String -> m ()
faintedXaxis s = svgClass "svg" s $ return ()

-- A small fainted line to use in performance graphs
faintedLineToAdjustGraph :: MonadWidget t m => String -> m ()
faintedLineToAdjustGraph c = svgClass "svg" c $ return ()

-- A dynamic bar for (Maybe Score)
scoreBar :: MonadWidget t m => (String,String,String, String) -> String -> Dynamic t (Maybe Score) -> m ()
scoreBar (class1,class2,class3, class4) key score  = elClass "div" class1 $ do
  bool <-  mapDyn (maybe False (const True)) score
  score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
  percent <- mapDyn asPercent score'
  let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100) (constDyn class2)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class3 >> dynBarCSS' percent (constDyn 100) (constDyn class2)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxis"
  xLabel class4 key
  mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

  -- A dynamic bar for (Maybe Score)
scoreBarV :: MonadWidget t m => (String,String,String, String) -> String -> Dynamic t (Maybe Score) -> m ()
scoreBarV (class1,class2,class3, class4) key score  = elClass "div" class1 $ do
  bool <-  mapDyn (maybe False (const True)) score
  score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
  percent <- mapDyn asPercent score'
  let b = dynScoreLabel (constDyn "scoreLabelV") percent >> dynBarCSS' percent (constDyn 100) (constDyn class2)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class3 >> dynBarCSS' percent (constDyn 100) (constDyn class2)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxis"
  xLabel class4 key
  mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabelV")

--inverted bars
scoreBarInvertedH :: MonadWidget t m => (String, String, String, String, String, String) -> String -> Dynamic t (Maybe Score) -> m()
scoreBarInvertedH (class1, class2, class3, class4, class5, class6) key currentScore = elClass "div" class1 $ do
  bool <- mapDyn (maybe False (const True)) currentScore
  currentScore' <- mapDyn (maybe (Score 0 0 0) id) currentScore
  let histScore = currentScore
  histScore' <- mapDyn (maybe (Score 0 0 0) id) histScore -- Dynamic t Int
  currentPercent <- mapDyn asPercent currentScore'
  histPercent <- mapDyn asPercent histScore'
  let b = dynScoreLabel (constDyn "scoreLabelCurrent") currentPercent >> dynScoreLabel (constDyn "scoreLabelHist") histPercent >> dynBarCSS' currentPercent (constDyn 100) (constDyn class2) >> dynBarCSSInv histPercent (constDyn 100) (constDyn class3)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class4 >> faintedLineCSS class5 >> dynBarCSS' currentPercent (constDyn 100) (constDyn class2) >> dynBarCSSInv histPercent (constDyn 100) (constDyn class3)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxisMirror"
  xLabel class6 key
  mapDyn questionsAsked currentScore' >>= dynCountLabel (constDyn "countLabel")

  -- A historical dynamic bar for (Maybe Score)
scoreBarH :: MonadWidget t m => (String,String,String, String, String, String) -> String -> Dynamic t (Maybe Score) -> m ()
scoreBarH (class1,class2,class3, class4, class5, class6) key currentScore = elClass "div" class1 $ do
  bool <-  mapDyn (maybe False (const True)) currentScore
  currentScore' <-  mapDyn (maybe (Score 0 0 0) id) currentScore -- Dynamic t Int
  let histScore = currentScore
  histScore' <- mapDyn (maybe (Score 0 0 0) id) histScore -- Dynamic t Int
  currentPercent <- mapDyn asPercent currentScore'
  histPercent <- mapDyn asPercent histScore'
  let b = dynScoreLabel (constDyn "scoreLabelCurrent") currentPercent >> dynScoreLabel (constDyn "scoreLabelHist") histPercent >> dynBarCSS' currentPercent (constDyn 100) (constDyn class2) >> dynBarCSS' histPercent (constDyn 100) (constDyn class3)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class4 >> faintedLineCSS class5 >> dynBarCSS' currentPercent (constDyn 100) (constDyn class2) >> dynBarCSS' histPercent (constDyn 100) (constDyn class3)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxis"
  xLabel class6 key
  mapDyn questionsAsked currentScore' >>= dynCountLabel (constDyn "countLabel")

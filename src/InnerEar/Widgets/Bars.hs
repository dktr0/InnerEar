{-# LANGUAGE OverloadedStrings #-}

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
import InnerEar.Types.Score as S
import InnerEar.Types.GScore as GS

dynBar :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> Dynamic t String -> m ()
dynBar percent barWidth c =  do
    class' <- mapDyn (singleton "class") c
    svgHeight <- mapDyn (* 150) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    let rotate' = constDyn (singleton "style" "transform:rotate(-180deg)")
    attrs <- mconcatDyn [class', svgHeight', rotate']
    svgDynAttr "svg" attrs $ return ()

backgroundColor :: Double -> String
backgroundColor x | x < 25.0              =  "background:linear-gradient(to top right, red,orange)"
                  | x > 25.0 && x <= 50.0 =  "background:linear-gradient(to top right, orange,yellow)"
                  | x > 50.0 && x <= 80.0 =  "background:linear-gradient(to top right, yellow,green)"
                  | x > 80.0             =  "background:linear-gradient(to top right, green,green)"
                  |otherwise              = "background:linear-gradient(to top right, gray,gray)"

dynBar' :: MonadWidget t m =>  Dynamic t Double -> Dynamic t String -> m ()
dynBar' percent c =  do
    class' <- mapDyn (singleton "class") c
    backgroundColor' <- mapDyn backgroundColor percent
    backgroundColor'' <- mapDyn (singleton "style") backgroundColor'
    svgWidth <- mapDyn (* 4) percent --scaling
    svgWidth' <- mapDyn (singleton "width" . show) svgWidth
    attrs <- mconcatDyn [class', svgWidth', backgroundColor'']
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
  percent <- mapDyn S.asPercent score'
  let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBar percent (constDyn 100) (constDyn class2)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class3 >> dynBar percent (constDyn 100) (constDyn class2)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxis"
  xLabel class4 key
  mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

--a gamified oval graph
ovalScoreBar :: MonadWidget t m => Dynamic t (Maybe GScore) ->  m ()
ovalScoreBar score = elClass "div" "ovalScoreWrapper" $ do
  score' <- mapDyn (maybe (GScore 0.0 50.0) id) score
  percent <- mapDyn GS.asPercent score' --Dynamic t Double
  dynBar' percent (constDyn "ovalScoreBar")
  return ()

-- A historical dynamic bar for (Maybe Score)
scoreBarH :: MonadWidget t m => (String,String,String, String, String, String) -> String -> Dynamic t (Maybe Score) -> m ()
scoreBarH (class1,class2,class3, class4, class5, class6) key currentScore = elClass "div" class1 $ do
  bool <-  mapDyn (maybe False (const True)) currentScore
  currentScore' <-  mapDyn (maybe (Score 0 0 0) id) currentScore -- Dynamic t Int
  let histScore = currentScore
  histScore' <- mapDyn (maybe (Score 0 0 0) id) histScore -- Dynamic t Int
  currentPercent <- mapDyn S.asPercent currentScore'
  histPercent <- mapDyn S.asPercent histScore'
  let b = dynScoreLabel (constDyn "scoreLabelCurrent") currentPercent >> dynScoreLabel (constDyn "scoreLabelHist") histPercent >> dynBar currentPercent (constDyn 100) (constDyn class2) >> dynBar histPercent (constDyn 100) (constDyn class3)
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS class4 >> faintedLineCSS class5 >> dynBar currentPercent (constDyn 100) (constDyn class2) >> dynBar histPercent (constDyn 100) (constDyn class3)
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  faintedXaxis "faintedXaxis"
  xLabel class6 key
  mapDyn questionsAsked currentScore' >>= dynCountLabel (constDyn "countLabel")

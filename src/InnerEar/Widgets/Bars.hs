module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad
import Data.Maybe (isJust)
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Labels
import InnerEar.Types.Score

dynBarCSS' :: MonadWidget t m =>  Dynamic t Double -> Dynamic t Float -> Dynamic t String -> m ()
dynBarCSS' percent barWidth c =  do
    class' <- mapDyn (singleton "class") c
    --let class' = constDyn (singleton "class" "svgBarContainer")
    svgHeight <- mapDyn (* 150) percent
    svgHeight' <- mapDyn (singleton "height" . show) svgHeight
    let rotate' = constDyn (singleton "transform" "rotate (180)")
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
scoreBar :: MonadWidget t m => String -> Dynamic t (Maybe Score) -> m ()
scoreBar key score  = elClass "div" "scoreBarWrapper" $ do
  bool <-  mapDyn (maybe False (const True)) score
  score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
  percent <- mapDyn asPercent score'
  let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainer")
  flippableDyn (return ()) b bool
  let b2 = emptyScoreLabel >> faintedLineCSS "svgFaintedLine" >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainer")
  flippableDyn b2 (return ()) bool
  faintedLineToAdjustGraph "faintedLineToAdjustGraph"
  xLabel "xLabel" key
  mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

scoreBar' :: MonadWidget t m => String -> Dynamic t (Maybe Score) -> m ()
scoreBar' key score  = elClass "div" "scoreBarWrapperFiveBand" $ do
    bool <-  mapDyn (maybe False (const True)) score
    score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
    percent <- mapDyn asPercent score'
    let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainerFiveBand")
    flippableDyn (return ()) b bool
    let b2 = emptyScoreLabel >> faintedLineCSS "svgFaintedLineFiveBand" >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainerFiveBand")
    flippableDyn b2 (return ()) bool
    faintedLineToAdjustGraph "faintedLineToAdjustGraph"
    xLabel "xLabelFiveBand" key
    mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

scoreBar'' :: MonadWidget t m => String -> Dynamic t (Maybe Score) -> m ()
scoreBar'' key score  = elClass "div" "scoreBarWrapperTenBand" $ do
    bool <-  mapDyn (maybe False (const True)) score
    score' <-  mapDyn (maybe (Score 0 0 0) id) score -- Dynamic t Int
    percent <- mapDyn asPercent score'
    let b = dynScoreLabel (constDyn "scoreLabel") percent >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainerTenBand")
    flippableDyn (return ()) b bool
    let b2 = emptyScoreLabel >> faintedLineCSS "svgFaintedLineTenBand" >> dynBarCSS' percent (constDyn 100) (constDyn "svgBarContainerTenBand")
    flippableDyn b2 (return ()) bool
    faintedLineToAdjustGraph "faintedLineToAdjustGraph"
    xLabel "xLabelTenBand" key
    mapDyn questionsAsked score' >>= dynCountLabel (constDyn "countLabel")

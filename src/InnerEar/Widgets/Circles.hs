{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Widgets.Circles where

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

backgroundColor :: Double -> String
backgroundColor x | x < 25.0              =  "orange"
                  | x > 25.0 && x <= 50.0 =  "yellow"
                  | x > 50.0 && x <= 80.0 =  "lightgreen"
                  | x > 80.0              =  "green"
                  |otherwise              =  "gray"

--a hollow circle
percentageCircle :: MonadWidget t m =>  Dynamic t Double -> m ()
percentageCircle percent =  do
  svgClass "svg" "pathContainer" $ do
    let d = constDyn (singleton "d" "M72 8.338 a 63.662 63.662 0 0 1 0 127.324 a 63.662 63.662 0 0 1 0 -127.324")
    let c = constDyn (singleton "class" "percentageCircle")
    percent' <- mapDyn (* (400/100)) percent  --scaling, need correction
    p <- mapDyn show percent' --Dynamic t String
    p' <- mapDyn (flip (++) " ") p
    p'' <- mapDyn (flip (++) "400") p'
    p''' <- mapDyn ("stroke-dasharray: " ++) p''
    circumference <- mapDyn (singleton "style") p'''
    backgroundColor' <- mapDyn backgroundColor percent
    backgroundColor'' <- mapDyn (singleton "stroke") backgroundColor'
    attrs <- mconcatDyn [d, c, circumference, backgroundColor'']
    svgDynAttr "path" attrs $ return ()

--a circular gamified graph
percentageCircleGraph :: MonadWidget t m => Dynamic t (Maybe GScore) -> m ()
percentageCircleGraph score = do
  score' <- mapDyn (maybe (GScore 0.0 50.0) id) score
  percent <- mapDyn GS.asPercent score'
  percentageCircle percent
  return ()

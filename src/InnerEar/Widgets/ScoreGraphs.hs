{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Widgets.ScoreGraphs where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad
import qualified Data.Text as T
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)

import InnerEar.Types.Frequency
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Lines
import InnerEar.Types.Score
import InnerEar.Types.GScore
import InnerEar.Widgets.Labels
import InnerEar.Widgets.Bars
import InnerEar.Widgets.Circles

--a list of frequencies (a list for the x axis)
xPoints :: [Double]
xPoints = [0, 1 .. 10000] -- :: [Frequency]

--a list to generate points in the y axis
linearGraphYPoints :: [Double]
linearGraphYPoints = fmap (\x -> x) xPoints

--a list to generate points in the y axis
steepGraphYpoints :: [Double]
steepGraphYpoints = fmap (\x -> (x*x)) xPoints

gradualGrapYpoints :: [Double]
gradualGrapYpoints = fmap (\x -> sqrt x) xPoints

flatGraphYpoints :: [Double]
flatGraphYpoints = fmap (\x -> 100) xPoints

--a graph generator for spectral shapes
graphGen :: MonadWidget t m => [Double] -> [Double] -> m ()
graphGen xPoints yPoints= do
      let xAndYPoints = zip xPoints yPoints
      shapeLine (constDyn "polyline") xAndYPoints
      return ()

--an oval graph generator
graphGenOval :: MonadWidget t m => Dynamic t (Maybe GScore) -> m ()
graphGenOval score = do
  gamifiedGraphLabel (constDyn "ovalGraphLabel") score
  ovalScoreBar score
  return ()

--a circular graph generator
graphGenCircular :: MonadWidget t m => Dynamic t (Maybe GScore) -> m ()
graphGenCircular score = do
  gamifiedGraphLabel (constDyn "circularGrapLabel") score
  percentageCircleGraph score
  return ()

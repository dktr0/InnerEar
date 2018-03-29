{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Widgets.SpecGraph where

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
import InnerEar.Widgets.Labels
import InnerEar.Widgets.Bars

--a framework for the shape graph
shapeGraphFrame :: MonadWidget t m =>  String -> String -> m ()
shapeGraphFrame xMainLabel graphLabel = do
  faintedYaxis "faintedYaxis"
  hzMainLabel "hzMainLabel" xMainLabel
  countMainLabel "countMainLabel" "#"
  percentageMainLabel "percentageMainLabel" "%"
  elClass "div" "graphLabel" $ text graphLabel
  return ()

--a list to generate points in the y axis
steepList :: [Double]
steepList = fmap ((\x -> 1/(x*x))) [1,2::Double .. ]

--a list of frequencies (a list for the x axis)
fList :: Double -> [Double]
fList freq = Prelude.filter (< 20000) $ take 10 $ fmap (* freq) [1,2 .. ] -- :: [Frequency]

--a generator of steep graphs
steepGraph :: MonadWidget t m => Double -> m ()
steepGraph freq = do
      let listOfPoints = zip (fList freq) steepList
      shapeLine (constDyn "polyline") listOfPoints
      return ()

--shapeLine [20,20, 40,25, 60,40, 80,120, 120,140, 200,180]

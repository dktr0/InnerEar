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

graphGen :: MonadWidget t m => [Double] -> [Double] -> m ()
graphGen xPoints yPoints= do
      let xAndYPoints = zip xPoints yPoints
      shapeLine (constDyn "polyline") xAndYPoints
      return ()

{--linearGraph :: MonadWidget t m =>  m ()
linearGraph = elClass "div" "specGraphWrapper" $ do
       let listOfPoints = zip (fList) linearGraphList
       shapeLine (constDyn "polyline") listOfPoints
       return ()
--}
--shapeLine [20,20, 40,25, 60,40, 80,120, 120,140, 200,180]

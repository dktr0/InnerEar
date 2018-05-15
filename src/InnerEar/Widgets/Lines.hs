module InnerEar.Widgets.Lines where

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
import InnerEar.Widgets.AnswerButton

--a helper function to take the parenthesis out
replaceEach ::  String -> String
replaceEach [] = []
replaceEach (x:xs)
   |x == '(' = replaceEach xs
   |x == ')' = replaceEach xs
   |otherwise = x:replaceEach xs

--a helper function to get "x,y x,y"
listToString :: [(Double,Double)] -> String
-- listToString [] = []
-- listToString (x:xs) = concat [replaceEach $ show $ x, " ", listToString xs]

listToString xs = concat $ Prelude.map (\(x,y) -> show x ++ "," ++ show y ++ " ") xs
--a helper function to get "x,y x,y"
listToString' :: [Double] -> String
listToString' [] = []
listToString' (x:y:zs) = concat [ show x, ",", show y, " ",  listToString' zs]

--a function to draw a polyline from a list of tuples of doubles
shapeLine ::  MonadWidget t m => Dynamic t String -> [(Double, Double)] -> m ()
shapeLine c listOfPoints = do
    svgClass "svg" "shapeContainer" $ do
      let listOfPoints' = listToString listOfPoints
      c' <- mapDyn (singleton "class") c
      let points = constDyn (singleton "points" listOfPoints')
      attrs <- mconcatDyn [c', points]
      svgDynAttr "polyline" attrs $ return ()

shapeLine' ::  MonadWidget t m => String -> Dynamic t AnswerButtonMode -> [(Double, Double)]  -> m ()
shapeLine' c  buttonMode listOfPoints = do
    dc <- mapDyn modeToClass buttonMode
    dc' <- mapDyn (singleton "class") dc
    --  svgDynAttr "svg" dc' $ do
    svgClass "svg" "shapeContainer" $ do
      let listOfPoints' = listToString listOfPoints
      let c' = singleton "class" c
      let points = singleton "points" listOfPoints'
      let attrs = mconcat [c',points]
      svgAttr "polyline" attrs $ return ()
--instructions dynamic t c

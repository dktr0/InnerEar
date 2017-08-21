module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad

import InnerEar.Widgets.Utility

--Dynamic "rect" element
rect :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Float -> Dynamic t Float -> Dynamic t String -> Dynamic t String -> m ()
rect posX posY width height style transform= do
  posX' <- mapDyn (singleton "x" . show) posX
  posY' <- mapDyn (singleton "y" . show) posY
  width' <- mapDyn (singleton "width" . show) width
  height' <- mapDyn (singleton "height" . show) height
  style' <- mapDyn (singleton "style") style
  transform' <- mapDyn (singleton "transform") transform
  m <- mconcatDyn [posX', posY', width', height', style', transform']
  svgDynAttr "rect" m $ return()

--Dynamic "rect" element with CSS style
rectDynCSS :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Float -> Dynamic t Float -> Dynamic t String -> Dynamic t String -> m ()
rectDynCSS posX posY width height transform cssClass = do
    cssClass' <- mapDyn (singleton "class") cssClass
    posX' <- mapDyn (singleton "x" . show) posX
    posY' <- mapDyn (singleton "y" . show) posY
    height' <- mapDyn (singleton "height" . show) height
    width' <- mapDyn (singleton "width" . show) width
    transform' <- mapDyn (singleton "transform") transform
    m <- mconcatDyn [posX', posY', width',height', cssClass', transform']
    svgDynAttr "rect" m $ return ()

rectDynCSS' :: MonadWidget t m => Dynamic t Float -> Dynamic t String -> m ()
rectDynCSS' height c = do
   height' <- mapDyn (singleton "height" . show) height
   c' <- mapDyn (singleton "class") c
   m <- mconcatDyn [height', c']
   svgDynAttr "rect" m $ return ()

--A dynamic bar
drawBar ::  MonadWidget t m =>  Dynamic t Int -> m ()
drawBar x =  do
   let svg = Just "http://www.w3.org/2000/svg"
   let svgAttrs = [("width", "100px")
                ,("height", "200px")
                ,("viewBox", "0 0 300 200")]
   elWith "svg" (ElConfig svg (fromList svgAttrs)) $ do
      elWith "rect" (ElConfig svg (fromList [("width", "100"), ("height", "100"), ("fill", "red")])) (return ())

--Another dynamic bar
drawBar' :: MonadWidget t m =>  Dynamic t Float -> m ()
drawBar' x  = do
    let m = fromList [("width","200px"),("height","200px"), ("viewBox", "0 0 300 200")]
    svgAttr "svg" m $ do
       let posX = constDyn  20 -- $ negate 100
       let posY = constDyn  50 -- $ negate 200
       let w = constDyn 50
       h <- mapDyn (*5) x
       let t = constDyn "rotate(0)"
       let s = constDyn "fill:green;stroke-width:5"
       rect posX posY w h s t

--A dynamic bar with css style and in-line attributes
drawBarCSS :: MonadWidget t m =>  Dynamic t Float -> Dynamic t Float -> m ()
drawBarCSS x y = do
  elClass "div" "flex-container" $ do
    svgClass "svg" "svgS" $ do
      let posX = constDyn $ negate 30
      let posY = constDyn $ negate 200
      h <- mapDyn (*200) x
      w <- mapDyn (*1) y
      let c = constDyn "test"
      let t = constDyn "rotate (180)"
      rectDynCSS posX posY w h t c

--Another dynamic bar with css style an height attribute
drawBarCSS' :: MonadWidget t m => Dynamic t Float -> m ()
drawBarCSS' x = do
  svgClass "svg" "svgS" $ do
   h <- mapDyn (*1) x
   let c = constDyn "rectStyle"
   rectDynCSS' h c

--Labels with CSS style to be used above bars
labelsForBars :: MonadWidget t m => String -> m ()
labelsForBars s = do
   elClass "div" "text" $ text (show s)
   return ()

-- A Dynamic label for percentage
dynPercentage :: MonadWidget t m => Dynamic t Int -> m ()
dynPercentage p = do
   p' <- mapDyn show p
   el "div" $ do
     dynText p'
     return ()

--A dynamic bar with a label, a button and CSS style
labelBarButton :: MonadWidget t m => String ->  Dynamic t String -> Dynamic t Float -> m (Event t ())
labelBarButton label buttonString barHeight = do
   labelsForBars label
   let barWidth = constDyn 30
   drawBarCSS barHeight barWidth
   question <- dynButton buttonString -- m (Event t ())
   return (question)

--A dynamic bar with a label, maybe percentage, maybe button label and maybe height
dynLabelBarButton :: MonadWidget t m => String ->  Dynamic t (Maybe Int) ->  Dynamic t (Maybe String) -> Dynamic t (Maybe Float) -> m (Event t ())
dynLabelBarButton label p buttonString barHeight = elClass "div" "barWrapper" $ do
    labelsForBars label
    let barWidth = constDyn 30
    barHeight' <- mapDyn (maybe 0.0 id) barHeight
    boolBar <- mapDyn (maybe False (const True)) barHeight
    flippableDyn (text " ") (drawBarCSS barHeight' barWidth) boolBar
    boolPercentage <- mapDyn (maybe False (const True)) p
    p' <- mapDyn (maybe 0 id) p -- Dynamic t Int
    flippableDyn (text " ") (dynPercentage p') boolPercentage
    el "div" $ do
      buttonString' <- mapDyn (maybe " " id) buttonString
      boolButton <- mapDyn (maybe False (const True)) buttonString --Dynamic t bool
      let emptyString = constDyn " "
      flippableDynE (dynButton emptyString) (dynButton buttonString') boolButton

--A dynamic label for Float percentage
dynPercentageFloat :: MonadWidget t m => Dynamic t String -> Dynamic t Float -> m ()
dynPercentageFloat c p = do
  c' <- mapDyn (singleton "class") c
  p' <- mapDyn show p
  elDynAttr "div" c' $ do
    dynText p'
    return ()

-- A dynamic label
dynLabelForBar :: MonadWidget t m => Dynamic t String -> Dynamic t String -> m ()
dynLabelForBar c label = do
  c' <- mapDyn (singleton "class") c
  elDynAttr "div" c' $ do
    dynText label
    return ()

--A dynamic label for count
dynCount :: MonadWidget t m => Dynamic t String -> Dynamic t Int -> m ()
dynCount c count = do
  c' <- mapDyn (singleton "class") c
  count' <- mapDyn show count
  elDynAttr "div" c' $ do
    dynText count'
    return ()

performanceBar :: MonadWidget t m => Dynamic t Float -> Dynamic t String -> Dynamic t Int -> m ()
performanceBar percentage label count =  do
  dynPercentageFloat (constDyn "percentageClass") percentage
  dynLabelForBar (constDyn "dynLabelForBarClass") label
  dynCount (constDyn "dynCountClass") count

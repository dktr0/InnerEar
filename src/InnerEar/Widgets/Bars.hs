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
  svgClass "svg" "svgS" $ do
     let posX = constDyn $ negate 30
     let posY = constDyn $ negate 200
     h <- mapDyn (*10) x
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

--A dynamic bar with a label, a button and CSS style
labelBarButton :: MonadWidget t m => String ->  Dynamic t String -> Dynamic t Float -> m (Event t ())
labelBarButton label buttonString barHeight = do
   labelsForBars label
   let barWidth = constDyn 30
   drawBarCSS barHeight barWidth
   question <- dynButton buttonString -- m (Event t ())
   return (question)































   {--
   svgAttr "svg" (singleton "class" "performanceBar") $
   x <-
   ...
   m <- mconcat []
   svgDynAttr
   --}
   --rulesof cominations in the sgs style
   --rect.perfomance bar

-- this was our original version from Friday (4 more versions follow)
-- after some discussion we should delete all but the final version below
dynButton' :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton' label = do
  let initialButton = return never -- m (Event t ())
  postBuildEvent <- getPostBuild -- m (Event t ())
  let postBuildLabel = tagDyn label postBuildEvent -- Event t String
  let postBuildButton = fmap button postBuildLabel
  let newButtons = fmap button $ updated label
  let newButtons' = leftmost [postBuildButton,newButtons]
  switchPromptlyDyn <$> widgetHold initialButton newButtons'

-- this second version uses "switchPromptly never" to flatten the result of "dyn"
dynButton'' :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton'' label = do
  x <- mapDyn button label -- Dyn (Event ())
  y <- dyn x -- Event t (event t)
  switchPromptly never y

--dyn :: MonadWidget t m => Dynamic t (m a) -> m (Event t a)

-- this third version is the same as the second but without the do notation
dynButton''' :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton''' label = mapDyn button label >>= dyn >>= switchPromptly never

-- this fourth version uses a more generic function "dynE" added to InnerEar.Widgets.Utility
dynButton'''' :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton'''' label = mapDyn button label >>= dynE


--dynE :: MonadWidget t m => Dynamic t (m (Event t a)) -> m (Event t a)

-- a final version that uses >=> from Control.Monad to compose together two a -> m b functions
--dynButton :: MonadWidget t m => Dynamic t String -> m (Event t ())
--dynButton = (mapDyn button) >=> dynE

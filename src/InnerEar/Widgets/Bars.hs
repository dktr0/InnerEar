module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg

data attributes = attributes {posX :: Float, posY :: Float, width :: Float, height :: Float, style :: String}


--labelBarButton :: MonadWidget t m => String -> Float -> Dynamic t (Maybe String) -> Dynamic t Float -> m

rect :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Int -> Dynamic t Int -> Dynamic t String -> m ()
rect posX posY width height style = do
  posX' <- mapDyn (singleton "posX" . show) posX
  posY' <- mapDyn (singleton "posY" . show) posY
  width' <- mapDyn (singleton "width" . show) width
  height' <- mapDyn (singleton "height" . show) height
  style' <- mapDyn (singleton "style") style
  m <- mconcatDyn [posX', posY', width', height', style']
  svgDynAttr "rect" m $ return()


--rectRoundedCorners :: MonadWidget t m => Dynamic t [attributes] -> m ()
--rectRoundCorners listOfAttr = listOfAttr


drawBar ::  MonadWidget t m =>  Dynamic t Int -> m ()
drawBar x =  do
 let svg = Just "http://www.w3.org/2000/svg"
 let svgAttrs = [("width", "100px")
                ,("height", "200px")
                ,("viewBox", "0 0 300 200")]
 --elDynAttr "200px" svgAttrs $ do el "height" $ x
 elWith "svg" (ElConfig svg (fromList svgAttrs)) $ do
   elWith "rect" (ElConfig svg (fromList [("width", "100"), ("height", "100"), ("fill", "red")])) (return ())

drawBar' :: MonadWidget t m =>  Dynamic t Int -> m ()
drawBar' x = do
    let m = fromList [("width","100px"),("height","200px"), ("viewBox", "0 0 300 200")]
    svgAttr "svg" m $ do
       let posX = constDyn 50
       let posY = constDyn 20
       let w = constDyn 50
       h <- mapDyn (*32) x
       let s = constDyn "fill:green;stroke-width:5"
       rect posX posY w h s

drawBar'' :: MonadWidget t m => Dynamic t Int -> m()
drawBar'' x = do
  let attr = fromList[("width", "100px"),("height", "200px"), ("viewBox", "0 0 300 200")]
  svgAttr "svg" attr $ do
     let posX = constDyn 50
     let posY = constDyn 20
     let w = constDyn 50
     h <- mapDyn(*32) x
     let s = constDyn "fill:blue"
     rect posX posY w h s





--Datam.map
--let x = constDyn value
--holDyn
--mapDyn from Reflex.Dynamic
--combineDyn

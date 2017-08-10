module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg

drawBar ::  MonadWidget t m =>  Dynamic t Int -> m ()
drawBar x =  do
 let svg = Just "http://www.w3.org/2000/svg"
 let svgAttrs = [("width", "100px")
                , ("height", "200px")
                , ("viewBox", "0 0 300 200") ]
 --elDynAttr "200px" svgAttrs $ do el "height" $ x
 elWith "svg" (ElConfig svg (fromList svgAttrs)) $ do
   elWith "rect" (ElConfig svg (fromList [("width", "100"), ("height", "100"), ("fill", "red")])) (return ())


rect :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t String -> m ()
rect w h s = do
  w' <- mapDyn (singleton "width" . show) w
  h' <- mapDyn (singleton "height" . show) h
  s' <- mapDyn (singleton "fill") s
  m  <- combineDyn Data.Map.union w' h' -- combining two maps
  m' <- combineDyn Data.Map.union m s' --combining other two maps
  svgDynAttr "rect" m' $ return()


drawBar' :: MonadWidget t m =>  Dynamic t Int -> m ()
drawBar' x = do
    let m = fromList [("width","400px"),("height","300px")]
    svgAttr "svg" m $ do
       let w = constDyn 50
       h <- mapDyn (*32) x
       let s = constDyn "green"
       rect w h s


drawBar'' :: MonadWidget t m => m()
drawBar'' = do
  let attr = fromList[("width", "300px"),("height", "200px")]
  svgAttr "svg" attr $ do
    let rectAttr = fromList[("width", "100"), ("height", "100"), ("fill", "blue")]
    svgAttr "rect" rectAttr $ return ()



--Datam.map
--let x = constDyn value
--holDyn
--mapDyn from Reflex.Dynamic
--combineDyn

module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg

rect :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> Dynamic t Int -> Dynamic t Int -> Dynamic t String -> m ()
rect posX posY width height style = do
  posX' <- mapDyn (singleton "posX" . show) posX
  posY' <- mapDyn (singleton "posY" . show) posY
  width' <- mapDyn (singleton "width" . show) width
  height' <- mapDyn (singleton "height" . show) height
  style' <- mapDyn (singleton "style") style
  m <- mconcatDyn [posX', posY', width', height', style']
  svgDynAttr "rect" m $ return()

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


--elDynAttr :: (...) => Text -> Dynamic t (Map Text Text) -> m a -> m a

dynButton :: MonadWidget t m => Dynamic t String -> m (Event t ())
dynButton label = do
  let initialButton = return never -- m (Event t ())
  postBuildEvent <- getPostBuild -- m (Event t ())
  let postBuildLabel = tagDyn label postBuildEvent -- Event t String
  let postBuildButton = fmap button postBuildLabel
  let newButtons = fmap button $ updated label
  let newButtons' = leftmost [postBuildButton,newButtons]
  switchPromptlyDyn <$> widgetHold initialButton newButtons'


labelBarButton :: MonadWidget t m => String -> Float -> Dynamic t String -> Dynamic t Int -> m (Event t ())
labelBarButton label 100.0 buttonString signal = do
    el "div" $ text (show label)
    drawBar' signal
    question <- dynButton buttonString -- m (Event t ())
    return (question)




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

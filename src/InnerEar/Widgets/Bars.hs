module InnerEar.Widgets.Bars where

import Reflex
import Reflex.Dom
import Data.Map

drawBar ::  MonadWidget t m =>  Dynamic t Int -> m ()
drawBar x =  do
 let svg = Just "http://www.w3.org/2000/svg"
 let svgAttrs = [("width", "100px")
                , ("height", "200px")
                , ("viewBox", "0 0 300 200") ]
 --elDynAttr "200px" svgAttrs $ do el "height" $ x
 elWith "svg" (ElConfig svg (fromList svgAttrs)) $ do
   elWith "rect" (ElConfig svg (fromList [("width", "100%"), ("height", "100%"), ("fill", "red")])) (return ())

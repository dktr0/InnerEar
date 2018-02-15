{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module InnerEar.Widgets.RangePicker where

import Control.Monad.IO.Class
import Reflex
import Reflex.Dom
import qualified GHCJS.Types as T
import GHCJS.DOM.Types
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.DOM.EventM

import InnerEar.Widgets.Utility

foreign import javascript unsafe
  "$r = $1.width"
  getWidth :: HTMLCanvasElement -> IO Float

foreign import javascript unsafe
  "$r = $1.height"
  getHeight :: HTMLCanvasElement -> IO Float

foreign import javascript unsafe
  "$1.getContext('2d').clearRect(0,0,$2,$3)"
  clearCanvas :: HTMLCanvasElement -> Float -> Float -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').rect($2,$3,$4,$5)"
  rect :: HTMLCanvasElement -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').stroke()"
  stroke :: HTMLCanvasElement -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').fill()"
  fill :: HTMLCanvasElement -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').fillStyle = $2"
  fillStyle :: HTMLCanvasElement -> T.JSString -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').strokeStyle = $2"
  strokeStyle :: HTMLCanvasElement -> T.JSString -> IO ()

data ClickEvents = Click Int | Drag Int | Release Int

data WidgetState = WidgetState {
    leftRange :: Float,
    rightRange :: Float,
    dragging :: Maybe (Either Float Float)
  }

f :: WidgetState -> ClickEvent -> WidgetState

widgetState <- foldDyn initialWidgetState f
dynRanges <- mapDyn (\w -> (leftRange w,rightRange w)  ) widgetState
let rangeEvents = updated dynRanges

rangePicker :: MonadWidget t m =>
  m (Dynamic t (Float,Float))

rangePicker = do
  (canvasEl,_) <- elClass' "canvas" "rangePickerClass" $ return ()
  let canvasEl' = _el_element canvasEl -- Element
  let c = castToHTMLCanvasElement canvasEl' -- HTMLCanvasElement
  clickEv <- wrapDomEvent canvasEl' (onEventName Click) mouseOffsetXY

  postBuild <- getPostBuild -- m (Event t ())
  let initialRangeEvent = fmap (const (0.25,0.75)) postBuild -- (Event t (Float,Float))
  let newLeftSelection = fmap (\(x,_) -> fromIntegral(x)/600.0 ) clickEv
  let newRangeEvent = fmap (\x -> (x,0.75)) newLeftSelection
  let rangeEvent = leftmost [initialRangeEvent,newRangeEvent]
  performEvent_ $ ffor rangeEvent $ \(r1,r2) -> liftIO $ do
    w <- getWidth c
    h <- getHeight c
    putStrLn $ show (w,h)
    strokeStyle c "#ffffff"
    fillStyle c "#ffffff"
    clearCanvas c w h
    rect c 0.0 0.0 (w*r1) h
    stroke c
    fill c
    rect c (w-((1.0-r2)*w)) 0.0  ((1.0-r2)*w) h
    stroke c
    fill c
  holdDyn (0,0) rangeEvent

{-# LANGUAGE  OverloadedStrings, JavaScriptFFI #-}

module InnerEar.Widgets.Canvas where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Reflex
import Reflex.Dom as D
import  GHCJS.Types (JSString)
import qualified GHCJS.DOM.Types as DT
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.DOM.EventM


import InnerEar.Widgets.Utility

data CanvasEvent = ClickEvent Double | DragEvent Double | ReleaseEvent Double

data RangeState = RangeState {
    leftRange :: Double,
    rightRange :: Double,
    dragging :: Maybe (Either Double Double)
  } deriving (Show)

updateRangeState::  CanvasEvent -> RangeState -> RangeState
updateRangeState (ClickEvent x) (RangeState l r Nothing)    -- catching when line has been clicked/ whether or not the click was in a 10% range
  | x>= l-0.05 && x <= l+0.05 = RangeState l r (Just $ Left x)
  | x>= r-0.05 && r <= r+0.05 = RangeState l r (Just $ Right x)
  | otherwise = RangeState l r Nothing
updateRangeState (ClickEvent x) (RangeState l r (Just _)) = RangeState l r Nothing  -- if somehow a click is registered when it is dragging, stop the drag
updateRangeState (DragEvent x) (RangeState l r Nothing) = RangeState l r Nothing  -- If get a drag event when a line isn't selected (hasn't been clicked before) don't drag
updateRangeState (DragEvent x) (RangeState l r (Just (Left _))) = RangeState l r (Just $ Left x)  -- update drag position if is dragging
updateRangeState (DragEvent x) (RangeState l r (Just (Right _))) = RangeState l r (Just $ Left x)
updateRangeState (ReleaseEvent x) (RangeState l r Nothing) = RangeState l r Nothing
updateRangeState (ReleaseEvent x) (RangeState l r (Just (Left _)))
  | x>=r = RangeState (r-0.02) r Nothing
  | otherwise = RangeState x r Nothing
updateRangeState (ReleaseEvent x) (RangeState l r (Just (Right _)))
  | x<=l = RangeState l (l+0.02) Nothing
  | otherwise = RangeState l x Nothing


--
rangePicker :: MonadWidget t m => (Double,Double) ->  m (Dynamic t (Double,Double))
rangePicker (l,r) = do
  let initialRangeState = RangeState l r Nothing
  (resize, canvasElement) <- resizeDetector $ liftM (_el_element . fst) $ elClass' "canvas" "startEndCanvas" $ return ()
  let htmlCanvasEl = DT.castToHTMLCanvasElement canvasElement -- HTMLCanvasElement

  --      iWidth <- liftIO (getWidth htmlCanvasEl)::MonadWidget t m  => m(Double)
  postBuild <- getPostBuild
  iWidthEv <- performEvent $ fmap (\_->liftIO (getWidth htmlCanvasEl)) postBuild -- (::MonadWidget t m => m (Event t Double))
  resizeEv <- performEvent $ fmap (\_->liftIO (getWidth htmlCanvasEl)) resize -- (::MonadWidget t m => m (Event t Double))
  canvasWidth <- holdDyn 0.5 $ leftmost [iWidthEv, resizeEv]

  clickEv <- liftM ((attachDynWith (\x y-> ClickEvent $ (fromIntegral y)/x) canvasWidth) . fmap fst) $ wrapDomEvent canvasElement (onEventName D.Mousedown) mouseOffsetXY
  dragEv <- liftM ((attachDynWith (\x y-> DragEvent $ (fromIntegral y)/x) canvasWidth) . fmap fst) $ wrapDomEvent canvasElement (onEventName D.Drag) mouseOffsetXY
  releaseEv <- liftM ((attachDynWith (\x y-> ReleaseEvent $ (fromIntegral y)/x) canvasWidth) . fmap fst) $ wrapDomEvent canvasElement (onEventName D.Mouseup) mouseOffsetXY

  -- clickEv <- liftM ( (<$>) (ClickEvent . fst)) $ wrapDomEvent canvasElement (onEventName D.Click) mouseOffsetXY
  -- dragEv <- liftM ( (<$>) (DragEvent . fst)) $ wrapDomEvent canvasElement (onEventName D.Drag) mouseOffsetXY
  -- releaseEv <- liftM ( (<$>) (ReleaseEvent . fst)) $ wrapDomEvent canvasElement (onEventName D.Drag) mouseOffsetXY
  -- let dragEv = never
  -- let clickEv = never
  -- let releaseEv = never
  rangeState <- foldDyn updateRangeState  initialRangeState (leftmost [clickEv, dragEv, releaseEv])
  mapDyn show rangeState >>= dynText
  -- performEvent_ $ fmap (\x -> liftIO $ drawRange x htmlCanvasEl) (updated rangeState)
  postBuild <- getPostBuild
  performEvent_ $ fmap (liftIO . drawRange htmlCanvasEl) (leftmost [updated rangeState, initialRangeState <$ postBuild])
  forDyn rangeState $ \x -> (leftRange x, rightRange x)

drawRange:: DT.HTMLCanvasElement -> RangeState -> IO ()
drawRange canvas (RangeState l r drag)  = do
  clearCanvas canvas
  w <- getWidth canvas
  h <- getHeight canvas
  strokeStyle canvas "#ffff00"
  fillRect canvas (l*w) 0 4 h
  fillRect canvas (r*w) 0 4 h



foreign import javascript unsafe
  "$1.getContext('2d').strokeStyle = $2" strokeStyle :: DT.HTMLCanvasElement -> JSString -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').clearRect(0,0,$1.width,$1.height)" clearCanvas :: DT.HTMLCanvasElement  -> IO ()

foreign import javascript unsafe
  "$r = $1.width" getWidth :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$r = $1.height" getHeight :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
 "$1.getContext('2d').fillRect($2,$3,$4,$5)" fillRect :: DT.HTMLCanvasElement -> Double -> Double -> Double -> Double -> IO ()

--
-- foreign import javascript unsafe
--   "$1.getContext('2d').strokeStyle = $2" strokeStyle :: DT.HTMLCanvasElement -> T.JSString -> IO ()
--
-- foreign import javascript unsafe
--   "$1.getContext('2d').clearRect(0,0,$1.width,$1.height)" clearCanvas :: DT.HTMLCanvasElement  -> IO ()
--
-- foreign import javascript unsafe
--   "$r = $1.width" getWidth :: DT.HTMLCanvasElement -> IO Float
--
-- foreign import javascript unsafe
--   "$r = $1.height" getHeight :: DT.HTMLCanvasElement -> IO Float
--
-- foreign import javascript unsafe
--  "$1.getContext('2d').fillRect($2,$3,$4,$5)" fillRect :: DT.HTMLCanvasElement -> Float -> Float -> Float -> Float -> IO ()

--   postBuild <- getPostBuild -- m (Event t ())
--   let initialRangeEvent = fmap (const (0.25,0.75)) postBuild -- (Event t (Int,Int))
--   let newLeftSelection = fmap (\(x,_) -> fromIntegral(x)/600.0 ) clickEv
--   let newRangeEvent = fmap (\x -> (x,0.75)) newLeftSelection
-- let rangeEvent = leftmost [initialRangeEvent,newRangeEvent]

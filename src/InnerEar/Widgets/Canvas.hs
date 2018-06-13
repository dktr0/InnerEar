{-# LANGUAGE  OverloadedStrings, JavaScriptFFI #-}

module InnerEar.Widgets.Canvas (
  rangeSelect,
  drawSource,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Map
import Data.Bool (bool)
import Reflex
import Reflex.Dom as D
import GHCJS.Types (JSString)
import qualified GHCJS.DOM.Types as DT
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.DOM.EventM

import Reflex.Synth.Spec

import InnerEar.Types.Sound
import InnerEar.Widgets.Utility

data CanvasEvent = ClickEvent {value::Double} | DragEvent {value::Double} | ReleaseEvent {value::Double} deriving (Show)

toPercent :: CanvasEvent -> Double -> CanvasEvent
toPercent (ClickEvent a) b = ClickEvent (a/b)
toPercent (DragEvent a) b = DragEvent (a/b)
toPercent (ReleaseEvent a) b = ReleaseEvent (a/b)

data RangeState = RangeState {
    leftRange :: Double,
    rightRange :: Double,
    dragging :: Maybe (Either Double Double)
  } deriving (Show)

updateRangeState :: CanvasEvent -> RangeState -> RangeState
updateRangeState (ClickEvent x) (RangeState l r Nothing)    -- catching when line has been clicked/ whether or not the click was in a 10% range
  | x>= l-0.05 && x <= l+0.05 = RangeState l r (Just $ Left x)
  | x>= r-0.05 && x <= r+0.05 = RangeState l r (Just $ Right x)
  | otherwise = RangeState l r Nothing
updateRangeState (ClickEvent x) (RangeState l r (Just _)) = RangeState l r Nothing  -- if somehow a click is registered when it is dragging, stop the drag
updateRangeState (DragEvent x) (RangeState l r Nothing) = RangeState l r Nothing  -- If get a drag event when a line isn't selected (hasn't been clicked before) don't drag
updateRangeState (DragEvent x) (RangeState l r (Just (Left _))) = RangeState l r (Just $ Left x)  -- update drag position if is dragging
updateRangeState (DragEvent x) (RangeState l r (Just (Right _))) = RangeState l r (Just $ Left x)
updateRangeState (ReleaseEvent x) (RangeState l r Nothing) = RangeState l r Nothing -- If hasn't been dragging...
updateRangeState (ReleaseEvent x) (RangeState l r (Just (Left _)))
  | x>=r = RangeState (r-0.02) r Nothing
  | otherwise = RangeState x r Nothing
updateRangeState (ReleaseEvent x) (RangeState l r (Just (Right _)))
  | x<=l = RangeState l (l+0.02) Nothing
  | otherwise = RangeState l x Nothing


--

rangeSelect :: MonadWidget t m => Dynamic t Bool -> (Double, Double) -> m (Dynamic t (Double, Double))
rangeSelect visible (l,r) = do
  let initialRangeState = RangeState l r Nothing
  (canvasElement,canvasPostBuild) <- liftM (\(a,b)->(_el_element a,b)) $ elClass' "canvas" "startEndCanvas" $ getPostBuild
  let htmlCanvasEl = DT.castToHTMLCanvasElement canvasElement -- HTMLCanvasElement
  postBuild <- getPostBuild
  iWidth <- liftIO (getOffsetWidth htmlCanvasEl)::MonadWidget t m => m Double
  iWidthEv <- performEvent $ fmap (\_-> liftIO (getOffsetWidth htmlCanvasEl)) canvasPostBuild -- (::MonadWidget t m => m (Event t Double))
  holdDyn "no event" (fmap show iWidthEv) >>= dynText
  clickEv <- liftM ( fmap (ClickEvent . fromIntegral . fst )) $ wrapDomEvent canvasElement (onEventName D.Mousedown) mouseOffsetXY
  dragEv <- liftM (fmap (DragEvent . fromIntegral . fst)) $ wrapDomEvent canvasElement (onEventName D.Drag) mouseOffsetXY
  releaseEv <- liftM (fmap (ReleaseEvent . fromIntegral . fst)) $ wrapDomEvent canvasElement (onEventName D.Mouseup) mouseOffsetXY
  getCanvasOffsetWidth <- performEvent $ fmap (\_ -> liftIO (getOffsetWidth htmlCanvasEl)) $ leftmost [clickEv, dragEv, releaseEv]
  canvasWidth <- holdDyn iWidth $ leftmost [getCanvasOffsetWidth,iWidthEv] --, iWidthEv]
  -- mapDyn (("canvasWidth: "++) . show) canvasWidth >>= dynText
  rangeState <- foldDyn updateRangeState  initialRangeState (attachDynWith (flip toPercent) canvasWidth $ fmap snd $ ffilter fst $ attachDyn visible $ leftmost [clickEv, dragEv, releaseEv])
  postBuild <- getPostBuild
  performEvent_ $ fmap (\(visible,r) -> liftIO $ if visible then drawRange htmlCanvasEl r else return ()) (attachDyn visible $ leftmost [updated rangeState, initialRangeState <$ postBuild])
  performEvent_ $ fmap (\(r,visible)-> liftIO (if visible then drawRange htmlCanvasEl r else clearCanvas htmlCanvasEl)) $ attachDyn rangeState $ leftmost [updated visible, tagDyn visible canvasPostBuild]
  forDyn rangeState $ \x -> (leftRange x, rightRange x)

drawRange :: DT.HTMLCanvasElement -> RangeState -> IO ()
drawRange canvas (RangeState l r drag)  = do
  clearCanvas canvas
  w <- getWidth canvas
  h <- getHeight canvas
  fillStyle canvas "#ffff00"
  fillRect canvas (l*w) 0 4 h
  fillRect canvas (r*w-4) 0 4 h -- Minus 4 (width of rectangle) so that can still see it when r = 1
  fillStyle canvas "rgba(255,255,0,0.4)"
  fillRect canvas 0 0 (l*w) h
  fillRect canvas (r*w) 0 (w-r*w) h

drawSource :: DT.HTMLCanvasElement -> SoundSource -> IO ()
drawSource canvas src = error "Not yet implemented! drawSource"

foreign import javascript unsafe
  "$1.getContext('2d').fillStyle = $2" fillStyle :: DT.HTMLCanvasElement -> JSString -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').clearRect(0,0,$1.width,$1.height)" clearCanvas :: DT.HTMLCanvasElement  -> IO ()

foreign import javascript unsafe
  "$r = $1.width" getWidth :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$r = $1.offsetWidth" getOffsetWidth :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$r = $1.height" getHeight :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
 "$1.getContext('2d').fillRect($2,$3,$4,$5)" fillRect :: DT.HTMLCanvasElement -> Double -> Double -> Double -> Double -> IO ()

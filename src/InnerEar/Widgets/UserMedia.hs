module InnerEar.Widgets.UserMedia where

import Reflex
import Reflex.Dom
import Reflex.Synth.Types
import Reflex.Synth.Synth
import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
import Reflex.Dom.Contrib.Widgets.Common
import qualified GHCJS.DOM.Types as G
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import GHCJS.DOM.EventM(mouseX)
import Text.Read(readMaybe)
import qualified Data.Map as M


import InnerEar.Types.Score
import InnerEar.Widgets.Utility


-- takes the inputId used for a LoadedFile, an event triggering when to
-- redraw the waveform, and provides a widget for setting loopstart, loopend, and loop
-- for a LoadedFile
waveformWidget::MonadWidget t m => String -> Event t () -> m (Dynamic t PlaybackParam)
waveformWidget inputId event= do
  (canvasEl,_) <- elClass' "canvas" "waveformCanvas" (return ())
  let canvasElement = _el_element canvasEl
  performEvent_ $ fmap liftIO $ fmap (const $ renderAudioWaveform inputId $ G.castToHTMLCanvasElement canvasElement) event -- redraw wavefor mon same canvas each event
  clickEv <- wrapDomEvent canvasElement (onEventName Click) (mouseX)
  pos <- holdDyn 0 clickEv
  mapDyn (("clickX:  "++) . show) pos >>= dynText
  start <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step"] ["number","0.1"])
  end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step"] ["number","0.1"])
  startVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double) ) (_textInput_value start)
  endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value end)
  param <- combineDyn (PlaybackParam) startVal endVal
  mapDyn (\x->x False) param



userMediaWidget::MonadWidget t m => String -> m (Dynamic t Source)
userMediaWidget s = do
  bufferLoadEv <- bufferInput s
  playbackParam <- waveformWidget s bufferLoadEv
  mapDyn (((flip NodeSource) 2) . BufferNode . LoadedFile s) playbackParam
  



--userMediaWidget'::MonadWidget t m => String -> Dynamic t Filter -> m ()
--userMediaWidget' s filt = do
--  bufferLoadEv <- bufferInput s
--  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
--           (WidgetConfig {_widgetConfig_initialValue= Just 1
--                         ,_widgetConfig_setValue = never
--                         ,_widgetConfig_attributes = constDyn M.empty})
--  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
--  let source = NodeSource (BufferNode $ LoadedFile s) 2
--  dynSound <- combineDyn (\x f-> if x ==1 then Sound source else FilteredSound source f) radioSelection filt
--  soundEv <- button "play"
--  performSound $ tagDyn dynSound soundEv

--userMediaWidget::MonadWidget t m => String -> Dynamic t Filter -> m ()
--userMediaWidget s filt = do
--  elClass "audio" s (return ())
--  bufferLoadEv <- bufferInput' s
--  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ [(1::Int,"Natural"),(2,"With filter")])
--  			(WidgetConfig {_widgetConfig_initialValue= Just 1
--  						 ,_widgetConfig_setValue = never
--  						 ,_widgetConfig_attributes = constDyn M.empty})
--  radioSelection <- mapDyn (maybe 2 id) (_hwidget_value radioWidget)
--  dynNode <- combineDyn (\x f-> if x==1 then GainNode 1 else FilterNode f) radioSelection filt
--  let source = NodeSource (MediaNode s) 2
--  loadedSound <- toggle False $ (True <$) bufferLoadEv
--  --gate:: Behavior Bool -> Event a -> Event a
--  let updateNodeEv = gate (current loadedSound) $ updated dynNode
--  holdAndConnectSound source updateNodeEv



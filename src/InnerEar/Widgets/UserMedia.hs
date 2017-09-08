module InnerEar.Widgets.UserMedia where

import Data.Bool (bool)
import qualified Data.Map as M
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


import InnerEar.Types.Score
import InnerEar.Widgets.Utility


-- takes the inputId used for a LoadedFile, an event triggering when to
-- redraw the waveform, and provides a widget for setting loopstart, loopend, and loop
-- for a LoadedFile
--waveformWidget::MonadWidget t m => String -> Event t () -> m (Dynamic t PlaybackParam)
--waveformWidget inputId event= elClass "div" "waveformWidget" $ do
--  (canvasEl,_) <- elClass' "canvas" "waveformCanvas" (return ())
--  let canvasElement = _el_element canvasEl
--  performEvent_ $ fmap liftIO $ fmap (const $ renderAudioWaveform inputId $ G.castToHTMLCanvasElement canvasElement) event -- redraw wavefor mon same canvas each event
--  clickEv <- wrapDomEvent canvasElement (onEventName Click) (mouseX)
--  pos <- holdDyn 0 clickEv
--  mapDyn (("clickX:  "++) . show) pos >>= dynText
--  start <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step"] ["number","0.1"])
--  end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step"] ["number","0.1"])
--  startVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double) ) (_textInput_value start)
--  endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value end)
--  param <- combineDyn (PlaybackParam) startVal endVal
--  mapDyn (\x->x False) param



userMediaWidget::MonadWidget t m => String -> Dynamic t String -> m (Dynamic t Source)
userMediaWidget inputId dynClass = elDynClass "div" dynClass $ do

  let attrs = FileInputConfig $ constDyn $ M.fromList $ zip ["accept","id"] ["audio/*",inputId]
  input <- elClass "div" "browseButton" $ fileInput attrs
  let loadEv = (() <$) $ updated $ _fileInput_value input

  -- create canvas
  canvasEl <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
  let canvasElement = _el_element canvasEl

  (stopEv,start,end,loop) <- elClass "div" "bufferControls" $ do
      stopEv<- button "stop"
      text "start "
      start <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
      text " end "
      end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
      text "loop"
      loop <- liftM _checkbox_value $ checkbox False def
      return (stopEv, start, end, loop)

  -- Load and draw the buffer when file has changed
  performEvent_ $ fmap (liftIO . const (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stopEv

  --Calculate the playbackParam
  clickEv <- wrapDomEvent canvasElement (onEventName Click) (mouseX)
  pos <- holdDyn 0 clickEv

  startVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value start)
  endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value end)
  param <- combineDyn (PlaybackParam) startVal endVal
  combineDyn (\x l-> ((flip NodeSource) $ Nothing) $ BufferNode $ LoadedFile inputId $ x l) param loop
--userMediaWidget::MonadWidget t m => String -> m (Dynamic t Source)
--userMediaWidget s = elClass "div" "userMediaWidget" $ do
--  bufferLoadEv <- bufferInput s
--  playbackParam <- waveformWidget s bufferLoadEv
--  mapDyn (((flip NodeSource) 2) . BufferNode . LoadedFile s) playbackParam

pinkNoiseOrFileSourceWidget :: MonadWidget t m => String -> m (Dynamic t Source)
pinkNoiseOrFileSourceWidget sourceID = elClass "div" "sourceWidget" $ do
  userFileSource <- userMediaWidget sourceID (constDyn "waveformWidget")
  let conf = (WidgetConfig {_widgetConfig_initialValue= Just (1::Int)
                         ,_widgetConfig_setValue = never
                         ,_widgetConfig_attributes = constDyn M.empty})
  text "Select the sound source: "
  radioWidget <- radioGroup (constDyn "radioWidget") (constDyn $ zip [1,2] ["Loaded file","pinknoise"]) conf
  radioWidgetSelection <-  mapDyn (maybe 1 id) (_hwidget_value radioWidget)
  combineDyn (\i s-> if i == 1 then s else ((flip NodeSource) $ Just 2) $ BufferNode $ File "pinknoise.wav") radioWidgetSelection userFileSource

sourceWidget::MonadWidget t m => String -> m (Dynamic t Source)
sourceWidget sourceID = elClass "div" "sourceWidget" $ do
  let staticSources = M.fromList $ zip [0::Int,1] $ fmap ((flip NodeSource) (Just 2) . BufferNode) [File "pinknoise.wav",File "whitenoise.wav"]
  let ddMap = constDyn $  M.fromList $ zip [(0::Int)..] ["Pink noise", "White noise", "Load a sound file"]
  dd <- elClass "div" "soundSourceDropdown" $ do
    text "Sound source: "
    dropdown 0 ddMap $ def -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  let ddVal = _dropdown_value dd
  dynClass <- mapDyn (bool ("nothingSourceWidget") ("waveformWidget") . (==2)) ddVal
  userFileSource <- userMediaWidget sourceID dynClass
  ddMapVal <- mapDyn (\x-> M.insert 2 x staticSources) userFileSource
  dynSource<-combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
  return (dynSource)


-- Event for reference, event for question, dynamic source
soundWidget'::MonadWidget t m => String -> m ( Event t (), Event t (), Dynamic t Source)
soundWidget' inputId = elClass "div" "soundWidget" $ do
  (playRef, playQ, stop) <- elClass "div" "playStopButtons" $ do
    ref <-  buttonClass "Listen to reference sound" "referenceSoundButton"
    q <- buttonClass "Listen to question" "questionSoundButton"
    s <- buttonClass "Stop" "stopSoundButton"
    return (ref, q, s)
  source <- sourceWidget inputId
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stop
  return (playRef, playQ, source)


  -- Event for reference, event for question, dynamic source
soundWidget::MonadWidget t m => String -> m ( Event t (), Event t (), Dynamic t Source)
soundWidget inputId = elClass "div" "soundWidget" $ do
  playRef <- elClass "div" "playReference" $ button "Listen to Reference Sound"
  playQ <- elClass "div" "playQuestion" $ button "Listen to Question"

  source <- elClass "div" "sourceWidget" $ do
    let staticSources = M.fromList $ zip [0::Int,1] $ fmap ((flip NodeSource) (Just 2) . BufferNode) [File "pinknoise.wav",File "whitenoise.wav"]
    let ddMap = constDyn $  M.fromList $ zip [(0::Int)..] ["Pink noise", "White noise", "Load a sound file"]
    -- Source selection dropdown
    ddVal  <- elClass "div" "soundSourceDropdown" $ do
      text "Sound source: "
      dd <- dropdown 0 ddMap $ def -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
      return $ _dropdown_value dd
    userFileSource <- userMediaWidget "inputId" (constDyn "userMediaWidget")
    ddMapVal <- mapDyn (\x-> M.insert 2 x staticSources) userFileSource
    combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
  return (playRef, playQ, source)
    -- loadedFileWidget

    --
    -- let attrs = FileInputConfig $ constDyn $ M.fromList $ zip ["accept","id"] ["audio/*",inputId]
    --   input <- elClass "div" "fileInputButton" $ fileInput attrs
    --   let ev = (() <$) $ updated $ _fileInput_value input
    --   return (ddVal, input, ev)
    --
    -- (loop,startVal,endVal)<-elClass "div" "soundFileControl" $ do
    --   (start,end) <- elClass "div" "startEndInput" $ do
    --     text "start "
    --     start <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
    --     text " end "
    --     end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
    --     startVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double) ) (_textInput_value start)
    --     endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value end)
    --     return (startVal,endVal)
    --   cb <- elClass "div" "loopCheckbox" $ do
    --     text "loop"
    --     c <-checkbox False def
    --     return $ _checkbox_value c
    --   return (cb, startVal, endVal)
    --
    --   -- create canvas
    --   canvasEl <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
    --   let canvasElement = _el_element canvasEl
    --
    --   -- Load and draw the buffer when file has changed
    --   performEvent_ $ fmap (liftIO . const (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
    --
    --   --Calculate the playbackParam
    --   clickEv <- wrapDomEvent canvasElement (onEventName Click) (mouseX)
    --   pos <- holdDyn 0 clickEv
    --
    --
    --   param <- combineDyn (PlaybackParam) startVal endVal
    --   combineDyn (\x l-> ((flip NodeSource) $ Nothing) $ BufferNode $ LoadedFile inputId $ x l) param loop
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --   userFileSource <- userMediaWidget sourceID dynClass
    --   ddMapVal <- mapDyn (\x-> M.insert 2 x staticSources) userFileSource
    --   dynSource<-combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
    --   return (dynSource)
    --
    --
    -- source <- sourceWidget inputId
    -- performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stop
    -- return (playRef, playQ, source)

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

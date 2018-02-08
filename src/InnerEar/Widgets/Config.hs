{-# LANGUAGE RecursiveDo #-}
module InnerEar.Widgets.Config where

import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Maybe(fromJust, isJust)
import Text.Read(readMaybe)
import qualified GHCJS.DOM.Types as G
-- import Reflex.Dom.Contrib.Widgets.Common (canvasEl)
import Reflex
import Reflex.Dom
import Control.Monad (liftM)
-- import Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)
-- import Reflex.Dom.Contrib.Widgets.Common

import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.Utility(radioWidget,elClass')
import Reflex.Synth.Types
import Reflex.Synth.Synth

-- Verify this but for css styling, what goes in the config panel is 1 div with class "configWidget" and 2 internal divs:
--    one div with the class "radioConfigWidget" -> wrapping the radio configuration widget
--    one div with the class "sourceWidget" -> wrapping the sound source selection widget

dynRadioConfigWidget:: (MonadWidget t m, Eq c, Show c, Ord c) => String -> Map Int (String,Source) -> Int ->  Map String c -> c -> m(Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget inputID sourceMap iSource configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  (source,playReference) <- sourceWidget'' inputID sourceMap iSource
  return (config, source, Nothing <$ playReference)
--

configWidget:: (MonadWidget t m, Eq c) => String -> Map Int (String,Source) -> Int ->  Map Int (String,c) -> c -> m(Dynamic t c, Dynamic t Source, Event t (Maybe a))
configWidget inputID sourceMap iSource configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configMap iConfig
  (source,playReference) <- elClass "div" "sourceWidget" $ sourceWidget''' inputID sourceMap iSource
  return (config, source, Nothing <$ playReference)

exerciseConfigWidget::(MonadWidget t m,Eq c)=> Map Int (String,c) -> c -> m (Dynamic t c)
exerciseConfigWidget configMap iConfig = do
  let index = maybe 0 id $ foldrWithKey (\k v b -> if (isJust b) then b else if (snd v == iConfig) then Just k else Nothing) Nothing configMap --uhg
  let ddConfig = DropdownConfig never (constDyn empty)
  dd <- dropdown index (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd

isLoadedFile::Source -> Bool
isLoadedFile (NodeSource (BufferNode (LoadedFile _ _)) _) = True
isLoadedFile _ = False

getPlaybackParam::Source -> Maybe PlaybackParam
getPlaybackParam (NodeSource (BufferNode (LoadedFile _ x)) _) = Just x
getPlaybackParam _ = Nothing

sourceWidget''':: MonadWidget t m => String -> Map Int (String, Source) -> Int -> m (Dynamic t Source, Event t ())
sourceWidget''' inputId sourceMap iSource = mdo
  (source, loadEv) <- elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown iSource (constDyn $ fmap fst sourceMap) $ DropdownConfig never (constDyn empty)
    s <- mapDyn (\x-> snd $ fromJust $ Data.Map.lookup x sourceMap) $ _dropdown_value dd  -- probably a better way to do this that doesn't use 'fromJust'
    isUserFile <- mapDyn isLoadedFile s
    let staticAttr = fromList [("accept","audio/*"),("id",inputId)]
    inputAttrs <- mapDyn (\x-> if (not x) then Data.Map.insert "hidden" "true" staticAttr else staticAttr) isUserFile
    input <- fileInput $ FileInputConfig inputAttrs
    let loadEv = (() <$) $ updated $ _fileInput_value input
    return (s,loadEv)
  -- canvas <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
  -- (canvas,_) <- elClass' "canvas" "waveformCanvas" (return ())
  -- let canvasElement = _el_element canvas
  -- performEvent_ $ fmap (liftIO . const (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
  --
  -- (startEndCanvas,_) <- elClass' "canvas" "startEndCanvas" (return())
  -- let startEndChangeEv = fmapMaybe getPlaybackParam $ updated source'
  -- performEvent_ $ fmap (\x-> liftIO $ (drawStartEnd x (G.castToHTMLCanvasElement $ _el_element startEndCanvas))) startEndChangeEv
  sourceCanvasWidget (updated source)  ((<$) inputId loadEv) (updated playbackParam)

  (playReference, stopEv, playbackParam) <- elClass "div" "bufferControls" $ do
    play <- button "►"
    stop <- button "◼"
    initialParams <- mapDyn (maybe (PlaybackParam 0 1 False) id . getPlaybackParam) source
    text "loop"
    loop <- liftM _checkbox_value $ checkbox False $ CheckboxConfig (fmap loop $ updated initialParams) (constDyn empty)
    let numberInputAttrs = constDyn $ fromList [("step","0.01"), ("max","1"), ("min","0"),("class","numberInput"),("type","number")]
    text "start "
    start <- textInput $ TextInputConfig "number" "0" (fmap (show . start)$ updated initialParams) numberInputAttrs
    startVal <- mapDyn (maybe 0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value start)
    text " end "
    end <- textInput $ TextInputConfig "number" "1" (fmap (show . end) $ updated initialParams) numberInputAttrs
    endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (_textInput_value end)
    param <- combineDyn PlaybackParam startVal endVal >>= combineDyn (flip ($)) loop
    return (play, stop, param)
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stopEv
  source' <- combineDyn  (\s p-> case s of (NodeSource (BufferNode (LoadedFile a _)) b) -> NodeSource (BufferNode $ LoadedFile a p) b; otherwise-> s ) source playbackParam
  mapDyn (show) source' >>= dynText
  return (source', playReference)


sourceCanvasWidget:: MonadWidget t m => Event t Source -> Event t String -> Event t PlaybackParam -> m ()
sourceCanvasWidget src loadEv paramChange = elClass "div" "sourceCanvasWrapper" $  do
  (canvas,_) <- elClass' "canvas" "waveformCanvas" (return ())
  let canvasElement = _el_element canvas
  -- performEvent_ $ fmap (\s ->liftIO  (drawSource s $ G.castToHTMLCanvasElement canvasElement)) (leftmost [updated src, tagDyn src loadEv])
  performEvent_ $ fmap (\inputId -> liftIO $ (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
  performEvent_ $ fmap (\s ->liftIO  (drawSource s $ G.castToHTMLCanvasElement canvasElement)) src

  performEvent_ $ fmap (liftIO . const clog) loadEv
  performEvent_ $ fmap (liftIO . const clog2) src

  (startEndCanvas,_) <- elClass' "canvas" "startEndCanvas" (return())
  performEvent_ $ fmap (\x-> liftIO $ (drawStartEnd x (G.castToHTMLCanvasElement $ _el_element startEndCanvas))) paramChange
  return ()

foreign import javascript safe "console.log('loadEvent ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^' )" clog:: IO ()
foreign import javascript safe "console.log('src event^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^' )" clog2:: IO ()

  -- paramsEv <- mapDyn getPlaybackParam src >>= updated
  -- (canvas,_) <- elClass' "canvas" "waveformCanvas" (return ())
  -- let canvasElement = _el_element canvas
  -- performEvent_ $ fmap (\s -> liftIO $  (drawCanvas s $ G.castToHTMLCanvasElement canvasElement)) (updated src)
  -- -- clickEv <- wrapDomEvent (_el_element e) (onEventName Click) mouseOffsetXY
  -- return src



sineSourceConfig::(MonadWidget t m) => String -> Map String Double -> Double -> m (Dynamic t Double, Dynamic t Source, Event t (Maybe a))
sineSourceConfig inputID configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  playReference <-elClass "div" "sourceWidget" $ do
    el "div" $ text "Source:  Sine wave 300Hz"
    ev <- button "Listen to reference"
    canvasEl <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
    -- performEvent $ fmap liftIO $ fmap (drawSineWave (G.castToHTMLCanvasElement $ _el_element canvasEl)) $ updated config
    liftIO $ drawSineWave (G.castToHTMLCanvasElement $ _el_element canvasEl)
    -- mapDyn (drawSineWave (G.castToHTMLCanvasElement $ _el_element canvasEl)) config
    return ev
  -- the source is rather arbitrary here - not really selecting any source here anyway so the render function should ignore it
  return (config, constDyn $ NodeSource (OscillatorNode $ Oscillator Sine 300 0) $ Just 2 , Nothing <$ playReference)

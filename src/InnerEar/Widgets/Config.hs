module InnerEar.Widgets.Config where

import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Maybe(fromJust)
import qualified GHCJS.DOM.Types as G
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
-- configWidget:: (MonadWidget t m) => String -> Map Int (String,Source) -> Int ->  Map Int (String,c) -> Int -> m(Dynamic t c, Dynamic t Source, Event t (Maybe a))
-- configWidget inputID sourceMap iSource configMap iConfig = elClass "div" "configWidget" $ do
--   config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configMap iConfig
--   (source,playReference) <- elClass "sourceWidget" $ sourceWidget sourceMap iSource
--   return (config, source, Nothing <$ playReference)

exerciseConfigWidget::(MonadWidget t m )=> Map Int (String,c) -> Int -> m (Dynamic t c)
exerciseConfigWidget configMap iConfig = do
  let ddConfig = DropdownConfig never (constDyn empty)
  dd <- dropdown iConfig (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd

isLoadedFile::Source -> Bool
isLoadedFile (NodeSource (BufferNode (LoadedFile _ _))) _) = True
isLoadedFile _ = False

getPlaybackParam::Source -> Maybe PlaybackParam
getPlaybackParam (NodeSource (BufferNode (LoadedFile _ _))) x) = Just x
getPlaybackParam _ = Nothing

sourceWidget:: MonadWidget t m => String -> Map Int (String, Source) -> Int -> m (Dynamic t Source, Event t ())
sourceWidget inputId sourceMap iSource = do
  (source, loadEv) <- elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown iSource (constDyn $ fmap fst sourceMap) $ DropdownConfig never (constDyn empty)
    s <- mapDyn (\x-> snd $ fromJust $ Data.Map.lookup x sourceMap) $ _dropdown_value dd  -- probably a better way to do this that doesn't use 'fromJust'
    isUserFile <- mapDyn isLoadedFile s
    inputAttrs <- mapDyn (\x-> fromList [("accent","audio/*"),("id",inputId),(if x then "" else "hidden","")]) isUserFile
    input <- fileInput $ FileInputConfig inputAttrs
    let loadEv = (() <$) $ updated $ _fileInput_value input
    return (s,loadEv)

  canvas <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
  let canvasElement = _el_element canvasEl
  performEvent_ $ fmap (liftIO . const (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv


  (playReference, stopEv, playbackParam) <- elClass "div" "bufferControls" $ do
      play <- button "►"
      stop <- button "◼"
      initialParams <- mapDyn (maybe (PlaybackParam 0 1 False) id . getPlaybackParam) source
      text "loop"
      loop <- liftM _checkbox_value $ checkbox False $ CheckboxConfig (fmap loop $ updated initialParams) (constDyn empty)
      text "start "
      start <- textInput $ TextInputConfig "number" "0" (fmap (show . start) $ updated initialParams) (constDyn $ fromList [("step","0.01"),("class","startEndNumberInput")])
      startVal <- mapDyn (maybe 0 id . ((readMaybe)::String->Maybe Double)) start
      text " end "
      end <- textInput $ TextInputConfig "number" "1" (fmap (show . end) $ updated initialParams) (constDyn $ fromList [("step","0.01"),("class","startEndNumberInput")])
      end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
      endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) end
      param <- combineDyn PlaybackParam startVal endVal >>= combineDyn (flip ($)) loop
      return (play, stop, param)
    performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stopEv

    -- TODO - swap out the new/changed 'playback param' from what the dropdown list has in case that loadedFiel is chosen in dropdwon.
    combineDyn ()



sourceWidget'':: MonadWidget t m => String -> M.Map Int (String,Source) -> Int -> m (Dynamic t Source, Event t ())
sourceWidget'' inputId sourceMap iSource = elClass "div" "sourceWidget" $ mdo
  let staticSources = M.map snd sourceMap
  let loadFileMapKey = case M.keys sourceMap of ([]) -> 0; otherwise -> (+1) $ maximum $ M.keys sourceMap

  let ddMap = constDyn $ M.insert loadFileMapKey ("Load a sound file") $ M.map fst sourceMap
  -- Source selection dropdown
  ddVal  <- elClass "div" "soundSourceDropdown" $ do
    text "Sound source: "
    let ddConfig = DropdownConfig (loadFileMapKey <$ loadEv) (constDyn M.empty)
    dd <- dropdown iSource ddMap  ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
    return $ _dropdown_value dd
  isUserSource <- mapDyn (==loadFileMapKey) ddVal
  playReference <- elClass "div" "playReference" $ button "Listen to reference sound"
  (userFileSource, loadEv) <- userMediaWidget "inputId" isUserSource
  ddMapVal <- mapDyn (\x-> M.insert loadFileMapKey x staticSources) userFileSource
  source <- combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) $ playReference
  return (source, playReference)


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

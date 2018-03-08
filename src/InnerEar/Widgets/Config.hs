{-# LANGUAGE RecursiveDo #-}
module InnerEar.Widgets.Config where

import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Maybe(fromJust, isJust)
import Text.Read(readMaybe)
import qualified GHCJS.DOM.Types as G
import Reflex
import Reflex.Dom
import Control.Monad (liftM)

import InnerEar.Widgets.Canvas
import InnerEar.Widgets.UserMedia
import InnerEar.Widgets.Utility(radioWidget,elClass', hideableWidget)
import Reflex.Synth.Types
import Reflex.Synth.Synth

-- Verify this but for css styling, what goes in the config panel is 1 div with class "configWidget" and 2 internal divs:
--    one div with the class "radioConfigWidget" -> wrapping the radio configuration widget
--    one div with the class "sourceWidget" -> wrapping the sound source selection widget

configWidget:: (MonadWidget t m, Eq c) => String -> Map Int (String,Source) -> Int -> String -> Map Int (String,c) -> c -> m(Dynamic t c, Dynamic t Source, Event t (Maybe a))
configWidget inputID sourceMap iSource configLabel configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configLabel configMap iConfig
  (source,playReference) <- elClass "div" "sourceWidget" $ sourceWidget inputID sourceMap iSource
  return (config, source, Nothing <$ playReference)

exerciseConfigWidget::(MonadWidget t m,Eq c)=> String -> Map Int (String,c) -> c -> m (Dynamic t c)
exerciseConfigWidget label configMap iConfig = do
  let index = maybe 0 id $ foldrWithKey (\k v b -> if (isJust b) then b else if (snd v == iConfig) then Just k else Nothing) Nothing configMap --uhg
  let ddConfig = DropdownConfig never (constDyn empty)
  text label
  dd <- dropdown index (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd


sourceWidget:: MonadWidget t m => String -> Map Int (String, Source) -> Int -> m (Dynamic t Source, Event t ())
sourceWidget inputId sourceMap iSource = mdo
  (ddSource, loadEv) <- elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown iSource (constDyn $ fmap fst sourceMap) $ DropdownConfig never (constDyn empty)
    s <- mapDyn (\x-> snd $ fromJust $ Data.Map.lookup x sourceMap) $ _dropdown_value dd  -- probably a better way to do this that doesn't use 'fromJust'
    isUserFile <- mapDyn isLoadedFile s
    let staticAttr = fromList [("accept","audio/*"),("id",inputId)]
    inputAttrs <- mapDyn (\x-> if (not x) then Data.Map.insert "hidden" "true" staticAttr else staticAttr) isUserFile
    input <- fileInput $ FileInputConfig inputAttrs
    let loadEv = (() <$) $ updated $ _fileInput_value input
    return (s,loadEv)
  source <- sourceCanvasWidget (ddSource)  ((<$) inputId loadEv) never
  (playReference, stopEv, l) <- elClass "div" "bufferControls" $ do
    play <- button "►"
    stop <- button "◼"
    initialParams <- mapDyn (maybe (PlaybackParam 0 1 False) id . getPlaybackParam) source
    text "loop"
    l <- liftM _checkbox_value $ checkbox False $ CheckboxConfig (fmap loop $ updated initialParams) (constDyn empty)
    return (play, stop, l)
  source' <- combineDyn (\s l -> case s of
    (NodeSource (BufferNode (LoadedFile a (PlaybackParam s e _))) dur) -> NodeSource (BufferNode $ LoadedFile a (PlaybackParam s e l)) dur
    otherwise -> s) source l   -- If it's a loaded file, swap the loop parameter in (no other sounds use the 'loop' thing)
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stopEv
  return (source', playReference)


sourceCanvasWidget:: MonadWidget t m => Dynamic t Source -> Event t String -> Event t PlaybackParam -> m (Dynamic t Source)
sourceCanvasWidget src loadEv paramChange = elClass "div" "sourceCanvasWrapper" $  do
  (canvas,_) <- elClass' "canvas" "waveformCanvas" (return ())
  let canvasElement = _el_element canvas
  postBuild <- getPostBuild
  performEvent_ $ fmap (\inputId -> liftIO $ (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
  performEvent_ $ fmap (\s ->liftIO  (drawSource s $ G.castToHTMLCanvasElement canvasElement)) (leftmost [updated src, tagDyn src postBuild])
  isLF<- mapDyn isLoadedFile src
  range <- hideableWidget isLF "" $ rangeSelect (0,1)
  combineDyn (\s (b,e) -> case s of (NodeSource (BufferNode (LoadedFile s (PlaybackParam _ _ l))) dur) -> NodeSource (BufferNode $ LoadedFile s (PlaybackParam b e l)) dur; otherwise -> s) src range

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
    liftIO $ drawSineWave (G.castToHTMLCanvasElement $ _el_element canvasEl)
    return ev
  return (config, constDyn $ NodeSource (OscillatorNode $ Oscillator Sine 300 0) $ Just 2 , Nothing <$ playReference)

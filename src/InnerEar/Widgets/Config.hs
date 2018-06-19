{-# LANGUAGE RecursiveDo #-}
module InnerEar.Widgets.Config where

import Control.Monad(liftM)
import Control.Monad.IO.Class (liftIO)

import Data.Map
import Data.Maybe(fromJust, isJust, listToMaybe)

import qualified GHCJS.DOM.Types as G

import Text.Read(readMaybe)

import InnerEar.Types.Sound
import InnerEar.Widgets.Canvas
import InnerEar.Widgets.Utility(radioWidget, elClass', hideableWidget, asapOrUpdated, combineDynIO)

import Reflex
import Reflex.Class
import Reflex.Dom
import Reflex.Synth.Synth


-- Verify this but for css styling, what goes in the config panel is 1 div with class "configWidget" and 2 internal divs:
--    one div with the class "radioConfigWidget" -> wrapping the radio configuration widget
--    one div with the class "sourceWidget" -> wrapping the sound source selection widget

-- sourceToSourceNodeSpec :: Source -> IO SourceNodeSpec
-- sourceToSourceNodeSpec SineOscillator' = return $ OscillatorNode Sine (Hz 300)
-- sourceToSourceNodeSpec UserSoundFile x = mkBuffer x

-- TODO inputID can be dropped anywhere it is used in here

configWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, SoundSourceConfigOption) -> Int -> String -> Map Int (String, c) -> Map String AudioBuffer -> c -> m (Dynamic t c, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
configWidget inputID sourceMap iSource configLabel configMap sysResources iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configLabel configMap iConfig
  (dynSrcSpec, playEv, stopEv) <- elClass "div" "sourceWidget" $ sourceSelectionWidget sysResources inputID sourceMap iSource
  return (config, dynSrcSpec, playEv, stopEv)

exerciseConfigWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, c) -> c -> m (Dynamic t c)
exerciseConfigWidget label configMap iConfig = do
  let index = maybe 0 id $ foldrWithKey (\k v b -> if (isJust b) then b else if (snd v == iConfig) then Just k else Nothing) Nothing configMap --uhg
  let ddConfig = DropdownConfig never (constDyn empty)
  text label
  dd <- dropdown index (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd

sourceSelectionWidget :: MonadWidget t m => Map String AudioBuffer -> String -> Map Int (String, SoundSourceConfigOption) -> Int -> m (Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
sourceSelectionWidget sysResources inputID choices defChoiceIdx =
  elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown defChoiceIdx (constDyn $ fmap fst choices) def

    dynSelOpt <- forDyn (value dd) $ \choiceIdx -> snd $ choices!choiceIdx
    inputAttrs <- forDyn dynSelOpt $ \opt ->
      let staticAttr = fromList [("accept", "audio/*"), ("id", inputID)] in
        case opt of
          UserProvidedResource -> staticAttr
          _ -> Data.Map.insert "hidden" "true" staticAttr

    userResourceInput <- fileInput $ FileInputConfig inputAttrs

    -- value userResourceInput :: Dynamic t [File] **but** because `multiple` is not specified
    -- as an attribute this list will never have more than 1. So we can map that to a `Maybe File`
    -- to make this more clear.
    dynSelFile <- mapDyn listToMaybe $ value userResourceInput

    selFileEv <- asapOrUpdated dynSelFile
    -- (Dynamic t (Maybe Buffer), Dynamic t BufferStatus)
    (dynBuffer, dynBufferStatus) <- mkBuffer selFileEv

    -- Dynamic t SoundSource
    dynSoundSrc <- combineDynIO (constructSoundSource sysResources) dynSelOpt dynBufferStatus
    dynConfig <- forDyn dynSoundSrc $ \src -> SoundSourceConfig {
        source = src,
        playbackRange = (0, 1),
        shouldLoop = False
      }

    (dynSrcCfg, playEv, stopEv) <- return dynConfig
      >>= sourceRangeConfigurationWidget
      >>= sourcePlaybackControlsWidget

    -- Take all of the configuration options and compile them import a configured spec
    dynSpec <- forDyn dynSrcCfg $ \cfg -> case source cfg of
      SourceLoading -> Nothing
      SourceUnderSpecified -> Nothing
      SourceError msg -> Nothing -- TODO display this error somewhere visible to the user
      SourceLoaded (AudioBufferSource d _) dur ->
        let params = BufferParams (fst $ playbackRange cfg) (snd $ playbackRange cfg) (shouldLoop cfg) in
          Just (AudioBufferSource d params, dur)
      SourceLoaded spec dur -> Just (spec, dur)

    return (dynSpec, playEv, stopEv)

constructSoundSource :: Map String AudioBuffer -> SoundSourceConfigOption -> BufferStatus -> IO SoundSource
constructSoundSource _ (Spec spec dur) _ = return $ SourceLoaded spec dur
constructSoundSource sysResources (Resource resourceId dur) _ =
  return $ SourceLoaded (AudioBufferSource (sysResources!resourceId) $ BufferParams 0 1 False) dur
constructSoundSource _ UserProvidedResource BufferUnderspecified = return SourceUnderSpecified
constructSoundSource _ UserProvidedResource BufferLoading = return SourceLoading
constructSoundSource _ UserProvidedResource (BufferError msg) = return $ SourceError msg
constructSoundSource _ UserProvidedResource (BufferLoaded bufData) = do
  dur <- bufferDuration bufData
  return $ SourceLoaded (AudioBufferSource bufData $ BufferParams 0 1 False) (Just dur)

-- | sourceSelectionConfigWidget configures a selected source.
sourceRangeConfigurationWidget :: MonadWidget t m => Dynamic t SoundSourceConfig -> m (Dynamic t SoundSourceConfig)
sourceRangeConfigurationWidget dynSrc =
  elClass "div" "sourceCanvasWrapper" $ do
    canvasElement <- G.castToHTMLCanvasElement <$> _el_element <$> fst <$> elClass' "canvas" "waveformCanvas" blank

    initialRange <- playbackRange <$> sample (current dynSrc)
    dynRangeSelectVisible <- forDyn dynSrc $ \s -> case source s of
      SourceLoaded (AudioBufferSource _ _) _ -> True
      otherwise -> False

    dynPlaybackRange <- rangeSelect dynRangeSelectVisible initialRange

    -- Draw the source asap and then redraw any time the source is updated
    dynLoadedSpec <- mapDyn source dynSrc
    specChangedEv <- asapOrUpdated dynLoadedSpec
    performEvent_ $ fmap (liftIO . drawSource canvasElement) specChangedEv

    combineDyn (\r cfg -> cfg { playbackRange = r }) dynPlaybackRange dynSrc

sourcePlaybackControlsWidget :: MonadWidget t m => Dynamic t SoundSourceConfig -> m (Dynamic t SoundSourceConfig, Event t (), Event t ())
sourcePlaybackControlsWidget dynSrc =
  elClass "div" "bufferControls" $ do
    playEv <- button "►"
    stopEv <- button "◼"

    text "Loop:"
    isLoopingSetNow <- shouldLoop <$> sample (current dynSrc)
    shouldLoopCheckbox <- checkbox isLoopingSetNow $ CheckboxConfig (fmap shouldLoop $ updated dynSrc) (constDyn empty)
    let dynIsLoopChecked = value shouldLoopCheckbox

    dynConfiguredSrc <- combineDyn (\l cfg -> cfg { shouldLoop = l }) dynIsLoopChecked dynSrc
    return (dynConfiguredSrc, playEv, stopEv)


-- Utilities to wrap buffer loading in reflex components

startLoadingBuffer :: MonadWidget t m => Event t G.File -> m (Event t Buffer, Event t BufferStatus)
startLoadingBuffer fileEv = do
  -- bufferEv :: Event t Buffer - a buffer ready to start loading it's file
  bufferEv <- performEvent $ ffor fileEv $ liftIO . createBuffer

  -- stateChangeEv :: Event t Buffer - tiggered on a status change
  stateChangeEv <- performEventAsync $ ffor bufferEv $ \buffer evTrigger ->
    liftIO $ startLoadingAndDecodingWithCallback buffer evTrigger

  -- statusEv' :: Event t BufferStatus - triggered on **relevant** status changes, hence the fmapMaybe
  statusEv <- performEvent $ fmap (liftIO . bufferStatus) stateChangeEv
  let statusEv' = fmapMaybe id statusEv

  return (stateChangeEv, statusEv')

-- | buffer creates a smart buffer for asynchronous loading of the most recent `Just` file fired
-- from the `Event t (Maybe File)`. Until the first occurance of the event, the buffer is `Nothing`.
-- The returned buffer status monitors the current state of the buffer.
mkBuffer :: MonadWidget t m => Event t (Maybe G.File) -> m (Dynamic t (Maybe Buffer), Dynamic t BufferStatus)
mkBuffer maybeFileEv = do
  (bufferEv, statusEv) <- startLoadingBuffer (fmapMaybe id maybeFileEv)
  dynBuffer <- holdDyn Nothing $ fmap Just bufferEv
  dynStatus <- holdDyn BufferUnderspecified statusEv
  return (dynBuffer, dynStatus)

-- sineSourceConfig :: (MonadWidget t m) => String -> Map String Double -> Double -> m (Dynamic t Double, Dynamic t Source, Event t (Maybe a))
-- sineSourceConfig inputID configMap iConfig = elClass "div" "configWidget" $ do
--   config <- elClass "div" "radioConfigWidget" $ do
--     text "Exercise Configuration:  "
--     (dynVal, _) <- radioWidget configMap (Just iConfig)
--     mapDyn (maybe iConfig id) dynVal
--   playReference <-elClass "div" "sourceWidget" $ do
--     el "div" $ text "Source:  Sine wave 300Hz"
--     ev <- button "Listen to reference"
--     canvasEl <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
--     liftIO $ drawSineWave (G.castToHTMLCanvasElement $ _el_element canvasEl)
--     return ev
--   return (config, constDyn $ NodeSource (OscillatorNode $ Oscillator Sine 300 0) $ Just 2 , Nothing <$ playReference)

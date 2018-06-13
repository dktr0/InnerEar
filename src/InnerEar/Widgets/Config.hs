{-# LANGUAGE RecursiveDo #-}
module InnerEar.Widgets.Config where

import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Maybe(fromJust, isJust, listToMaybe)
import Text.Read(readMaybe)
import qualified GHCJS.DOM.Types as G
import Reflex
import Reflex.Dom
import Reflex.Class
import Control.Monad(liftM)

import InnerEar.Widgets.Canvas
import InnerEar.Widgets.Utility(radioWidget, elClass', hideableWidget, asapOrUpdated)
import InnerEar.Types.Sound
import Reflex.Synth.Spec
import Reflex.Synth.Buffer


-- Verify this but for css styling, what goes in the config panel is 1 div with class "configWidget" and 2 internal divs:
--    one div with the class "radioConfigWidget" -> wrapping the radio configuration widget
--    one div with the class "sourceWidget" -> wrapping the sound source selection widget

-- sourceToSourceNodeSpec :: Source -> IO SourceNodeSpec
-- sourceToSourceNodeSpec SineOscillator' = return $ OscillatorNode Sine (Hz 300)
-- sourceToSourceNodeSpec UserSoundFile x = mkBuffer x

configWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, SoundSourceConfigOption) -> Int -> String -> Map Int (String, c) -> c -> m (Dynamic t c, Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
configWidget inputID sourceMap iSource configLabel configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configLabel configMap iConfig
  (dynSrcSpec, playEv, stopEv) <- elClass "div" "sourceWidget" $ sourceSelectionWidget inputID sourceMap iSource
  return (config, dynSrcSpec, playEv, stopEv)

exerciseConfigWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, c) -> c -> m (Dynamic t c)
exerciseConfigWidget label configMap iConfig = do
  let index = maybe 0 id $ foldrWithKey (\k v b -> if (isJust b) then b else if (snd v == iConfig) then Just k else Nothing) Nothing configMap --uhg
  let ddConfig = DropdownConfig never (constDyn empty)
  text label
  dd <- dropdown index (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd

sourceSelectionWidget :: MonadWidget t m => String -> Map Int (String, SoundSourceConfigOption) -> Int -> m (Dynamic t (Maybe (SourceNodeSpec, Maybe Time)), Event t (), Event t ())
sourceSelectionWidget inputID choices defChoiceIdx =
  elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown defChoiceIdx (constDyn $ fmap fst choices) def

    dynSelOpt <- forDyn (value dd) $ \choiceIdx -> snd $ choices!choiceIdx
    inputAttrs <- forDyn dynSelOpt $ \opt -> 
      let staticAttr = fromList [("accept", "audio/*"), ("id", inputID)] in
        case opt of
          UserProvidedResource -> Data.Map.insert "hidden" "true" staticAttr
          _ -> staticAttr

    userResourceInput <- fileInput $ FileInputConfig inputAttrs

    -- value userResourceInput :: Dynamic t [File] **but** because `multiple` is not specified
    -- as an attribute this list will never have more than 1. So we can map that to a `Maybe File`
    -- to make this more clear.
    dynSelFile <- mapDyn listToMaybe $ value userResourceInput

    selFileEv <- asapOrUpdated dynSelFile
    -- (Dynamic t (Maybe Buffer), Dynamic t BufferStatus)
    (dynBuffer, dynBufferStatus) <- buffer selFileEv

    -- Dynamic t SoundSource
    dynSoundSrc <- combineDyn constructSoundSource dynSelOpt dynBufferStatus
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
      SourceLoaded (AudioBufferSource d _) -> 
        let params = BufferParams (fst $ playbackRange cfg) (snd $ playbackRange cfg) (shouldLoop cfg) in
          Just $ AudioBufferSource d params
      SourceLoaded spec -> Just spec

    return (dynSpec, playEv, stopEv)

constructSoundSource :: SoundSourceConfigOption -> BufferStatus -> SoundSource
constructSoundSource (Spec spec) _ = SourceLoaded spec
constructSoundSource (Resource resourceId) _ = error "Not yet implemented: loadOption (Resource resourceId)" -- TODO needs implementation, grab local resource
constructSoundSource UserProvidedResource BufferUnderspecified = SourceUnderSpecified
constructSoundSource UserProvidedResource BufferLoading = SourceLoading
constructSoundSource UserProvidedResource (BufferError msg) = SourceError msg
constructSoundSource UserProvidedResource (BufferLoaded bufData) = SourceLoaded $ AudioBufferSource bufData $ BufferParams 0 1 False

-- | sourceSelectionConfigWidget configures a selected source.
sourceRangeConfigurationWidget :: MonadWidget t m => Dynamic t SoundSourceConfig -> m (Dynamic t SoundSourceConfig)
sourceRangeConfigurationWidget dynSrc =
  elClass "div" "sourceCanvasWrapper" $ do
    canvasElement <- G.castToHTMLCanvasElement <$> _el_element <$> fst <$> elClass' "canvas" "waveformCanvas" blank
    
    initialRange <- playbackRange <$> sample (current dynSrc)
    dynRangeSelectVisible <- forDyn dynSrc $ \s -> case source s of
      SourceLoaded (AudioBufferSource _ _) -> True
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

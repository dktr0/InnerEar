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
import InnerEar.Widgets.Utility(radioWidget, elClass', hideableWidget)
import InnerEar.Types.Sound
import Reflex.Synth.Spec
import Reflex.Synth.Utils


-- Verify this but for css styling, what goes in the config panel is 1 div with class "configWidget" and 2 internal divs:
--    one div with the class "radioConfigWidget" -> wrapping the radio configuration widget
--    one div with the class "sourceWidget" -> wrapping the sound source selection widget

-- sourceToSourceNodeSpec :: Source -> IO SourceNodeSpec
-- sourceToSourceNodeSpec SineOscillator' = return $ OscillatorNode Sine (Hz 300)
-- sourceToSourceNodeSpec UserSoundFile x = mkBuffer x

configWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, SoundSourceConfigOption) -> Int -> String -> Map Int (String, c) -> c -> m (Dynamic t c, Dynamic t (Maybe SourceNodeSpec), Event t (Maybe a))
configWidget inputID sourceMap iSource configLabel configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "exerciseConfigWidget" $ exerciseConfigWidget configLabel configMap iConfig
  (dynSrcSpec, playEv, stopEv) <- elClass "div" "sourceWidget" $ sourceSelectionWidget inputID sourceMap iSource
  return (config, dynSrcSpec, Nothing <$ playEv) 
  -- TODO return the stopEv as well. Also why is the playEv a `Maybe a`?

exerciseConfigWidget :: (MonadWidget t m, Eq c) => String -> Map Int (String, c) -> c -> m (Dynamic t c)
exerciseConfigWidget label configMap iConfig = do
  let index = maybe 0 id $ foldrWithKey (\k v b -> if (isJust b) then b else if (snd v == iConfig) then Just k else Nothing) Nothing configMap --uhg
  let ddConfig = DropdownConfig never (constDyn empty)
  text label
  dd <- dropdown index (constDyn $ fmap fst configMap) ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
  mapDyn (\x -> snd $ fromJust $ Data.Map.lookup x configMap) $ _dropdown_value dd

sourceSelectionWidget :: MonadWidget t m => String -> Map Int (String, SoundSourceConfigOption) -> Int -> m (Dynamic t (Maybe SourceNodeSpec))
sourceSelectionWidget inputID choices defChoiceIdx = do
  (dynSelOpt, dynSelFile) <- elClass "div" "sourceSelection" $ do
    text "Sound source: "
    dd <- dropdown defChoiceIdx (constDyn $ fmap fst choices) def

    dynSelOpt <- forDyn (value dd) $ \choiceIdx -> snd $ choices!!choiceIdx
    inputAttrs <- forDyn dynSelOpt $ \opt -> 
      let staticAttr = fromList [("accept", "audio/*"), ("id", inputID)] in
        case opt of
          UserProvidedResource -> Data.Map.insert "hidden" "true" staticAttr
          _ -> staticAttr

    userResourceInput <- fileInput $ FileInputConfig inputAttrs

    -- value userResourceInput :: Dynamic t [File] **but** because `multiple` is not specified
    -- as an attribute this list will never have more than 1. So we can map that to a `Maybe File`
    -- to make this more clear.
    -- (Dynamic t SoundSourceConfig, Dynamic t (Maybe File))
    return (dynSelOpt, fmap listToMaybe $ value userResourceInput)

  -- TODO this pattern is really nice where there could be any arbitrary source
  -- configuration widgets between the loaded and playback. This should be abstracted
  -- a bit more to better support new source options.
  return $ loadOption dynSelOpt dynSelFile 
    >>= sourceRangeConfigurationWidget
    >>= sourcePlaybackControlsWidget

loadOption :: MonadWidget t m => Dynamic t SoundSourceConfigOption -> Dynamic t (Maybe G.File) -> m (Dynamic t (Maybe SourceNodeSpec))
loadOption (Spec spec) _ = return $ constDyn $ Just spec
loadOption (Resource resourceId) _ = undefined -- TODO needs implementation, grab local resource
loadOption UserProvidedResource Nothing = return $ constDyn $ Nothing -- No spec until file is specified
loadOption UserProvidedResource (Just src) = do
  -- loadedEv :: Event t (Either String AudioBuffer)
  loadedEv <- loadAudioBufferFromFile src
  -- loadedSpecEv :: Event t (Maybe SourceNodeSpec)
  loadedSpecEv <- ffor loadedEv $ \res -> case res of
    Left errMsg -> Nothing -- TODO present this error somehow.
    Right buffer -> Just $ AudioBufferSource buffer $ BufferParams 0 1 False
  dynSpec <- holdDyn Nothing loadedSpecEv
  return $ holdDyn Nothing 

-- | sourceSelectionConfigWidget configures a selected source.
sourceRangeConfigurationWidget :: MonadWidget t m => Dynamic t (Maybe SourceNodeSpec) -> m (Dynamic t (Maybe SourceNodeSpec))
sourceRangeConfigurationWidget dynLoadedSpec =
  elClass "div" "sourceCanvasWrapper" $ do
    canvasElement <- G.castToHTMLCanvasElement <$> _el_element <$> elClass' "canvas" "waveformCanvas" blank
    
    dynPlaybackRange <- forDyn dynLoadedSpec $ \s -> case s of
      Nothing -> rangeSelect False (0, 1)
      Just Silent -> rangeSelect False (0, 1)
      Just (Oscillator _ _) -> rangeSelect False (0, 1)
      Just (AudioBufferSource _ (BufferParams start end _)) -> rangeSelect True (start, end)

    -- Try to redraw the source (no-op if Nothing) when the spec is loaded and when the canvas is ready to draw on
    postBuild <- getPostBuild
    performEvent_ $ fmap (maybe (return ()) (drawSource canvasElement)) $ leftmost [updated dynLoadedSpec, tagDyn dynLoadedSpec postBuild]

    return $ combineDyn configureSpecRange dynPlaybackRange dynLoadedSpec
  where
    configureSpecRange :: (Double, Double) -> Maybe SourceNodeSpec -> Maybe SourceNodeSpec
    configureSpecRange (start, end) (Just (AudioBufferSource buf (BufferParams _ _ loop))) = 
      Just $ AudioBufferSource buf $ BufferParams start end loop
    configureSpec _ = id

sourcePlaybackControlsWidget :: MonadWidget t m => Dynamic t (Maybe SourceNodeSpec) -> m (Dynamic t (Maybe SourceNodeSpec), Event t (), Event t ())
sourcePlaybackControlsWidget dynLoadedSpec =
  elClass "div" "bufferControls" $ do
    playEv <- button "►"
    stopEv <- button "◼"
    
    text "Loop:"
    isLoopingSetNow <- isLoopingSet <$> sample <$> current dynLoadedSpec
    loopCheckbox <- checkbox isLoopingSetNow $ CheckboxConfig (fmap isLoopingSet $ updated dynLoadedSpec) (constDyn empty)
    dynIsLoopChecked <- value loopCheckbox

    dynConfiguredSpec <- combineDyn configureSpecLooping dynIsLoopChecked dynLoadedSpec
    return (dynConfiguredSpec, playEv, stopEv)
  where 
    isLoopingSet :: Maybe SourceNodeSpec -> Bool
    isLoopingSet (Just (AudioBufferSource _ (BufferParams _ _ loop))) = loop
    isLoopingSet _ = False
    
    configureSpecLooping :: Bool -> Maybe SourceNodeSpec -> Maybe SourceNodeSpec
    configureSpecLooping looping (Just (AudioBufferSource buf (BufferParams s e _))) =
      Just $ AudioBufferSource buf $ BufferParams s e looping
    configureSpecLooping _ = id

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

{-# LANGUAGE RecursiveDo #-}
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




-- Returning a LoadedFile source, event for playing reference, event for when file is loaded
userMediaWidget::MonadWidget t m => String -> Dynamic t Bool -> m (Dynamic t Source, Event t (), Event t ())
userMediaWidget inputId isEnabled = do
  attrs <- mapDyn (bool (M.fromList $ zip ["class","disabled"] ["userMediaWidgetDisabled","disabled"]) (M.singleton "class" "userMediaWidget")) isEnabled

  elDynAttr "div" attrs $ do
    let attrsMap = M.fromList $ zip ["accept","id"] ["audio/*",inputId]
    attrs <- mapDyn (bool (M.insert "disabled" "disabled" attrsMap) attrsMap) isEnabled
    input <- elClass "div" "browseButton" $ fileInput $ FileInputConfig attrs
    let loadEv = (() <$) $ updated $ _fileInput_value input

    -- create canvas
    canvasEl <- elClass "div" "waveformWrapper" $ liftM fst $ elClass' "canvas" "waveformCanvas" (return ())
    let canvasElement = _el_element canvasEl

    (playEv, stopEv, loop) <- elClass "div" "bufferControls" $ do
        inputAttrs <- mapDyn (bool (M.singleton "disabled" "disabled")  M.empty) isEnabled
        playEv <- buttonDynAttrs "Play reference sound" () inputAttrs
        stopEv<- buttonDynAttrs "stop" () inputAttrs
        -- text "start "
        -- start <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
        -- text " end "
        -- end <- textInput $ def & textInputConfig_attributes .~ (constDyn $ M.fromList $ zip ["type","step","class"] ["number","0.01","startEndNumberInput"])
        text "loop"
        loop <- liftM _checkbox_value $ checkbox False $ def & checkboxConfig_attributes .~ inputAttrs
        return (playEv, stopEv, loop)

    -- Load and draw the buffer when file has changed
    performEvent_ $ fmap (liftIO . const (loadAndDrawBuffer inputId $ G.castToHTMLCanvasElement canvasElement)) loadEv
    performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) stopEv

    --Calculate the playbackParam
    clickEv <- wrapDomEvent canvasElement (onEventName Click) (mouseX)
    pos <- holdDyn 0 clickEv

    -- startVal <- mapDyn (maybe 0 id . ((readMaybe)::String->Maybe Double)) (start)
    -- endVal <- mapDyn (maybe 1.0 id . ((readMaybe)::String->Maybe Double)) (end)
    let startVal = constDyn (0::Double)
    let endVal = constDyn (1::Double)
    param <- combineDyn (PlaybackParam) startVal endVal
    s<-combineDyn (\x l-> ((flip NodeSource) $ Nothing) $ BufferNode $ LoadedFile inputId $ x l) param loop
    return (s,playEv, loadEv)

--
--   -- Event for reference, event for question, dynamic source
-- soundWidget::MonadWidget t m => String -> m ( Event t (), Event t (), Dynamic t Source)
-- soundWidget inputId = elClass "div" "soundWidget" $ do
--   playRef <- elClass "div" "playReference" $ button "Listen to Reference Sound"
--   playQ <- elClass "div" "playQuestion" $ button "Listen to Question"
--
--   source <- elClass "div" "sourceWidget" $ mdo
--     let staticSources = M.fromList $ zip [0::Int,1] $ fmap ((flip NodeSource) (Just 2) . BufferNode) [File "pinknoise.wav",File "whitenoise.wav"]
--     let ddMap = constDyn $  M.fromList $ zip [(0::Int)..] ["Pink noise", "White noise", "Load a sound file"]
--     -- Source selection dropdown
--     ddVal  <- elClass "div" "soundSourceDropdown" $ do
--       text "Sound source: "
--       let ddConfig = DropdownConfig (2 <$ loadEv) (constDyn M.empty)
--       dd <- dropdown 0 ddMap  ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
--       return $ _dropdown_value dd
--     isUserSource <- mapDyn (==2) ddVal
--     (userFileSource, loadEv) <- userMediaWidget "inputId" isUserSource
--     ddMapVal <- mapDyn (\x-> M.insert 2 x staticSources) userFileSource
--     combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
--   performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) $ leftmost [playRef,playQ]
--   return (playRef, playQ, source)

sourceWidget:: MonadWidget t m => String -> m (Dynamic t Source, Event t ())
sourceWidget inputId = elClass "div" "sourceWidget" $ mdo
  let staticSources = M.fromList $ zip [0::Int,1] $ fmap ((flip NodeSource) (Just 2) . BufferNode) [File "pinknoise.wav",File "whitenoise.wav"]
  let ddMap = constDyn $  M.fromList $ zip [(0::Int)..] ["Pink noise", "White noise", "Load a sound file"]
  -- Source selection dropdown
  ddVal  <- elClass "div" "soundSourceDropdown" $ do
    text "Sound source: "
    let ddConfig = DropdownConfig (2 <$ loadEv) (constDyn M.empty)
    dd <- dropdown 0 ddMap  ddConfig -- & dropdownConfig_attributes .~ (constDyn $ M.singleton "class" "soundSourceDropdown")
    return $ _dropdown_value dd
  isUserSource <- mapDyn (==2) ddVal
  (userFileSource,playReference, loadEv) <- userMediaWidget "inputId" isUserSource
  ddMapVal <- mapDyn (\x-> M.insert 2 x staticSources) userFileSource
  source <- combineDyn (\i m-> maybe (NodeSource (BufferNode $ File "pinknoise.wav") $ Just 2) id $ M.lookup i m) ddVal ddMapVal
  performEvent $ fmap liftIO $ fmap (const $ stopNodeByID inputId) $ playReference
  return (source, playReference)

module InnerEar.Widgets.Config where

import Control.Monad.IO.Class (liftIO)
import Data.Map
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



dynRadioConfigWidget::(MonadWidget t m, Eq c, Show c, Ord c) => String -> Map String c -> c  -> m (Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget inputID configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  (source,playReference) <- sourceWidget inputID
  return (config, source, Nothing <$ playReference)

-- @abstract the above to fit more functions....
dynRadioConfigWidget'::(MonadWidget t m, Eq c, Show c, Ord c) => String -> Map String c -> c  -> m (Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget' inputID configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  (source,playReference) <- sourceWidget' inputID
  return (config, source, Nothing <$ playReference)

dynRadioConfigWidget'':: (MonadWidget t m, Eq c, Show c, Ord c) => String -> Map Int (String,Source) -> Int ->  Map String c -> c -> m(Dynamic t c, Dynamic t Source, Event t (Maybe a))
dynRadioConfigWidget'' inputID sourceMap iSource configMap iConfig = elClass "div" "configWidget" $ do
  config <- elClass "div" "radioConfigWidget" $ do
    text "Exercise Configuration:  "
    (dynVal, _) <- radioWidget configMap (Just iConfig)
    mapDyn (maybe iConfig id) dynVal
  (source,playReference) <- sourceWidget'' inputID sourceMap iSource
  return (config, source, Nothing <$ playReference)


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

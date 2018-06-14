{-# LANGUAGE RecursiveDo, OverloadedStrings, DeriveDataTypeable #-}
module InnerEar.Widgets.Client where

import Control.Monad.IO.Class (liftIO)

import Data.Map(Map)

import Reflex
import Reflex.Dom

import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.Login
import InnerEar.Widgets.Navigation
import qualified InnerEar.WebSocket as WS
import Reflex.Synth.Synth

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- If the user is logged in as an authenticated user, it displays that.
-- If the user is not logged in, it displays fields to enter login values.

performSynth :: MonadWidget t m => Event t (Maybe (Synth ())) -> m ()
performSynth selectedSynth = mdo
  -- Behaviour t (Maybe Synthstance) - the previously instantiated synth
  prevSynthstance <- hold Nothing newSynthstances

  -- Event t (Maybe Synthstance, Maybe (Synth ())) - prev and current
  let synthstnacePair = attach prevSynthstance selectedSynth

  -- Event t (Maybe Synthstance)
  newSynthstances <- performEvent $ ffor synthstnacePair $ \(prev, curr) -> liftIO $ do
    -- Stop the old one if it existed
    maybe (return ()) stopSynthNow prev
    -- Start the new one if given
    maybe (return Nothing) (\s -> instantiateSynth s >>= startSynthNow >>= return . Just) curr

  return ()

clientWidget :: MonadWidget t m => Map String AudioBuffer -> m ()
clientWidget sysResources = elClass "div" "innerEar" $ mdo
  (wsRcvd,wsStatus) <- WS.reflexWebSocket wsSend
  (x,currentRole) <- elClass "div" "header" $ do
    elClass "div" "title" $ do
      text "Inner Ear"
    elClass "div" "login" $ loginWidget wsRcvd
  (y, synthEv) <- navigationWidget sysResources wsRcvd currentRole
  let wsSend = leftmost [x,y]
  performSynth synthEv
  return ()

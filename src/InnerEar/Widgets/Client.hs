{-# LANGUAGE RecursiveDo, OverloadedStrings, DeriveDataTypeable #-}
module InnerEar.Widgets.Client where

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
import Reflex.Synth.Types

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- If the user is logged in as an authenticated user, it displays that.
-- If the user is not logged in, it displays fields to enter login values.

clientWidget :: MonadWidget t m => m ()
clientWidget = elClass "div" "innerEar" $ mdo
  (wsRcvd,wsStatus) <- WS.reflexWebSocket wsSend
  (x,currentRole) <- elClass "div" "header" $ do
    elClass "div" "title" $ do
      text "Inner Ear"
    elClass "div" "login" $ loginWidget wsRcvd
  (y, synthEv) <- navigationWidget wsRcvd currentRole
  let wsSend = leftmost [x,y]
  performSynth synthEv
  return ()

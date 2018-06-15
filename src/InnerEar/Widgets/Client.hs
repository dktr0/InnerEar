{-# LANGUAGE RecursiveDo, OverloadedStrings, DeriveDataTypeable #-}
module InnerEar.Widgets.Client where

import Control.Monad
import Data.Maybe
import Reflex
import Reflex.Dom

import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.Utility
import InnerEar.Widgets.Login
import InnerEar.Widgets.Navigation
import qualified InnerEar.WebSocket as WS
import Reflex.Synth.Synth
import Reflex.Synth.Types

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- At first it displays the login widget plus a choice to continue without logging in.
-- Successfully logging in (or choosing to continue without logging in) loads
-- the excercise navigationWidget instead.

clientWidget :: MonadWidget t m => m ()
clientWidget = elClass "div" "innerEar" $ mdo
  (wsRcvd,wsStatus) <- WS.reflexWebSocket wsSend
  elClass "div" "title" $ text "Inner Ear"
  loginVisible <- holdDyn True $ leftmost [True <$ loggedIn, False <$ noLogin, True <$ loggedOut]
  navVisible <- mapDyn not loginVisible
  (loginRequests,currentRole) <- visibleWhen loginVisible $ elClass "div" "login" $ loginWidget wsRcvd
  let loggedIn = ffilter isJust $ updated currentRole
  let loggedOut = ffilter isNothing $ updated currentRole
  noLogin <- visibleWhen loginVisible $ button "Continue without logging in"
  (navRequests,sounds) <- visibleWhen navVisible $ navigationWidget wsRcvd currentRole
  let wsSend = leftmost [loginRequests,navRequests]
  performSound sounds
  return ()

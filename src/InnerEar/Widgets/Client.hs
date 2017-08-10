module InnerEar.Widgets.Client where

import Reflex
import Reflex.Dom

import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.Login
import InnerEar.Widgets.Navigation
import InnerEar.WebSocket

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- If the user is logged in as an authenticated user, it displays that.
-- If the user is not logged in, it displays fields to enter login values.

clientWidget :: MonadWidget t m => m ()
clientWidget = el "div" $ do
  el "div" $ text "Inner Ear"
  (wsRcvd,wsStatus) <- reflexWebSocket wsSend
  el "div" $ do
    text "wsStatus: "
    dynText wsStatus
  x <- loginWidget responses
  y <- navigationWidget responses
  let wsSend = leftmost [x,y]
  return ()

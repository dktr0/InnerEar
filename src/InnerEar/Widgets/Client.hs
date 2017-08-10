module InnerEar.Widgets.Client where

import Reflex
import Reflex.Dom

import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.Login
import InnerEar.Widgets.Navigation

-- | The clientWidget is the top-level widget in the Inner Ear web client.
-- If the user is logged in as an authenticated user, it displays that.
-- If the user is not logged in, it displays fields to enter login values.

clientWidget :: MonadWidget t m => Event t Response -> m (Event t Request)
clientWidget responses = el "div" $ do
  el "div" $ do
    text "Inner Ear"
  x <- loginWidget responses
  y <- navigationWidget responses
  return $ leftmost [x,y]

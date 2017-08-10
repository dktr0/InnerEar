module InnerEar.Widgets.CreateUser where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response

createUserWidget :: MonadWidget t m
  => Event t [Response] -> m (Event t Request,Event t ())
createUserWidget responses = el "div" $ do
  text "createuserpage placeholder"
  home <- button "back to splash page"
  return (never,home)

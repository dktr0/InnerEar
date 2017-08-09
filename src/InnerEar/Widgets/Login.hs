module InnerEar.Widgets.Login where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response

loginWidget :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
loginWidget responses = el "div" $ do
  text "loginpage placeholder"
  home <- button "back to splash page"
  return (never,home)

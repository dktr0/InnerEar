module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response

prototypeExercise :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  home <- button "back to splash page"
  return (never,home)

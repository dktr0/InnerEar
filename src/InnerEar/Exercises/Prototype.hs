module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Test

prototypeExercise :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  makeASound <- liftM ((FilteredSound (OscillatorSource (Oscillator Sawtooth 440) 2.0) (Filter Peaking 100.0 1.0 1.0)) <$) $ button "Make A Sound"
  performSound makeASound
  score <- count makeASound
  drawBar score
  home <- button "back to splash page"
  return (never,home)

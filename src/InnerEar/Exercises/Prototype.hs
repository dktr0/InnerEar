module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Widgets.Bars

prototypeExercise :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  makeASound <- liftM ((FilteredSound (Tone (Saw 440) 2.0) (PeakingFilter 100.0 1.0 1.0)) <$) $ button "Make A Sound"
  performSynth makeASound
  score <- count makeASound
  drawBar score
  drawBar' score
  drawBar''
  home <- button "back to splash page"
  return (never,home)

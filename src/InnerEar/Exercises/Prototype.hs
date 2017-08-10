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
  => Event t [Response] -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  makeASound <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 100 1 1)) <$) $ button "Pinknoise Peak 100 1 1"
  makeASound' <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 400 1 1)) <$) $ button "Pinknoise Peak 400 1 1"
  makeASound'' <- liftM ((FilteredSound (BufferSource (File "pinknoise.wav") 2.0) (Filter Peaking 900 1 1)) <$) $ button "Pinknoise Peak 900 1 1"
  performSound $ leftmost [makeASound,makeASound', makeASound'']
  score <- count makeASound
  drawBar score
  drawBar' score
  drawBar''
  home <- button "back to splash page"
  return (never,home)

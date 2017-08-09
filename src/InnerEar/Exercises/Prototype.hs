module InnerEar.Exercises.Prototype where

import Reflex
import Reflex.Dom
import Control.Monad

import InnerEar.Types.Request
import InnerEar.Types.Response
import Reflex.Synth.Synth


prototypeExercise :: MonadWidget t m
  => Event t Response -> m (Event t Request,Event t ())
prototypeExercise responses = el "div" $ do
  text "prototype exercise placeholder"
  makeASound <- liftM ((Synth (PinkNoise 2.0) (PeakingFilter 1000.0 1.0 1.0)) <$) $ button "Make A Sound"
  performSynth makeASound
  score <- count makeASound
  drawBar score -- drawBar :: (MonadWidget t m, Num b) => Dynamic t b -> m () -- MonadWidget t m => Dynamic t Int -> m ()
  home <- button "back to splash page"
  return (never,home)




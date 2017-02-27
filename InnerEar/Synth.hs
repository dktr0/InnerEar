module InnerEar.Synth where

import InnerEar.WebAudio.Types

class WebAudio a where
  createNode :: a -> IO WebAudioNode

data Filter = NoFilter |
              PeakingFilter Double Double Double -- Frequency Q Gain

instance WebAudio Filter where
  createNode (NoFilter) = createGainNode 1.0
  createNode (PeakingFilter f q g) = createPeakingFilter f q g

data Source = PinkNoise

instance WebAudio Source where
  createNode (PinkNoise dur) = do
    x <- createPinkNoiseNode
    y <- createAsrEnvelope 0.005 1.0 0.005
    connect x y

data Synth = Synth Source Filter

instance WebAudio Synth where
  createNode (Synth s f) = do
    x <- createNode s
    y <- createNode f
    connect x y
    dest <- getDestination
    connect y dest

-- an example of how this might be used with reflex-dom:

example :: m (Event t ())
example = do
  play <- button "play"
  let synthEvent = Synth PinkNoise (PeakingFilter 1500 1.4 6.0) <$ play
  performEvent_ synthEvent

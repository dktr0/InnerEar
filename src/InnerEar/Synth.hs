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

data Synth = NoSynth | Synth Source Filter

instance WebAudio Synth where
  createNode (NoSynth) = return NullAudioNode
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
  let synthAction = createNode (Synth PinkNoise (PeakingFilter 1500 1.4 6.0)) <$ play
  performEvent_ $ fmap liftIO synthAction

getNewQuestion :: StdGen -> IO (Synth,StdGen)
getNewQuestion gen = do
  (x,g) <- randomR (0,1) gen
  let s = if x==0 then (Synth PinkNoise NoFilter) else (Synth PinkNoise (PeakingFilter 1500 1.4 6.0))
  return (s,g)

example2 :: StdGen -> m ()
example2 gen = el "div" $ mdo
  newButton <- button "new"
  gen'' <- tagPromptlyDyn gen' newButton -- m (Event t StdGen)
  newQuestion <- performEvent $ fmap (liftIO . getNewQuestion) gen'' -- m (Event t (Synth,StdGen))
  synth <- holdDyn (NoSynth) $ fmap fst newQuestion -- m (Dynamic t Synth)
  gen' <- holdDyn gen $ fmap snd newQuestion -- m (Dynamic t StdGen)
  playButton <- button "play"
  synthAction <- tagPromptlyDyn synth playButton -- m (Event t Synth)
  performEvent_ $ fmap (liftIO . createNode) synthAction

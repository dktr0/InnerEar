module Reflex.Synth.Synth where

import Reflex.Synth.Types 
--import InnerEar.Types.Sound
import qualified Reflex.Synth.Foreign as F
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)


class WebAudio a where
  createGraph :: a -> IO WebAudioGraph


instance WebAudio Filter where
  createGraph (NoFilter) = createGain 1.0 >>= return . WebAudioGraph
  createGraph filt = createBiquadFilter filt >>= return . WebAudioGraph


instance WebAudio Source where
  createGraph (PinkNoiseSource dur) = do
    x <- createPinkNoise
    y <- createAsrEnvelope 0.005 dur 0.005 
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph
  createGraph (OscillatorSource osc dur) = do
    x <- createOscillator osc
    y <- createAsrEnvelope 0.005 dur 0.005
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph


instance WebAudio WebAudioGraph where
  createGraph = connectGraph

instance WebAudio Sound where
  createGraph (FilteredSound s f) = do
    source <- createGraph s
    filt <- createGraph f
    let graph = WebAudioGraph'' source filt
    connectGraph graph

createAudioContext::IO ()
createAudioContext = F.createAudioContext

createSound :: Sound -> IO (WebAudioGraph)
createSound (FilteredSound s f) = do
  sourceNode <- createGraph s
  filterNode <- createGraph f
  dest <- getDestination
  let graph = WebAudioGraph'' sourceNode (WebAudioGraph'' filterNode (WebAudioGraph dest))
  connectGraph graph

performSound:: MonadWidget t m => Event t Sound -> m ()
performSound event = do
  let n = fmap (\e-> do 
                      graph <- createGraph e
                      startGraph graph
                      ) event          -- Event t (IO ())
  performEvent_ $ fmap liftIO n






-- an example of how this might be used with reflex-dom:

--example :: m (Event t ())
--example = do
--  play <- button "play"
--  let synthAction = createNode (Synth PinkNoise (PeakingFilter 1500 1.4 6.0)) <$ play
--  performEvent_ $ fmap liftIO synthAction

--getNewQuestion :: StdGen -> IO (Synth,StdGen)
--getNewQuestion gen = do
--  (x,g) <- randomR (0,1) gen
--  let s = if x==0 then (Synth PinkNoise NoFilter) else (Synth PinkNoise (PeakingFilter 1500 1.4 6.0))
--  return (s,g)

--example2 :: StdGen -> m ()
--example2 gen = el "div" $ mdo
--  newButton <- button "new"
--  gen'' <- tagPromptlyDyn gen' newButton -- m (Event t StdGen)
--  newQuestion <- performEvent $ fmap (liftIO . getNewQuestion) gen'' -- m (Event t (Synth,StdGen))
--  synth <- holdDyn (NoSynth) $ fmap fst newQuestion -- m (Dynamic t Synth)
--  gen' <- holdDyn gen $ fmap snd newQuestion -- m (Dynamic t StdGen)
--  playButton <- button "play"
--  synthAction <- tagPromptlyDyn synth playButton -- m (Event t Synth)
--  performEvent_ $ fmap (liftIO . createNode) synthAction

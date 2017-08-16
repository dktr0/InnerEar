module Reflex.Synth.Synth where

import Reflex.Synth.Types 
--import InnerEar.Types.Sound
import qualified Reflex.Synth.Foreign as F
import qualified Data.Map as M
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)

class WebAudio a where
  createGraph :: a -> IO WebAudioGraph

instance WebAudio Filter where
  createGraph (NoFilter) = createGain 1.0 >>= return . WebAudioGraph
  createGraph filt = createBiquadFilter filt >>= return . WebAudioGraph

instance WebAudio Buffer where
  createGraph (File path) = createBufferNode (File path) >>= return . WebAudioGraph

instance WebAudio Source where
  createGraph (NodeSource node dur) = do
    x <- createNode node
    y <- createAsrEnvelope 0.005 dur 0.005
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph
  createGraph (OscillatorSource osc dur) = do
    x <- createOscillator osc
    y <- createAsrEnvelope 0.005 dur 0.005
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph
  createGraph (BufferSource b dur) = do
    x <- createBufferNode b
    y <- createAsrEnvelope 0.005 dur 0.005
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph
  createGraph (MediaSource) = createMediaNode >>= return . WebAudioGraph


instance WebAudio WebAudioGraph where
  createGraph = connectGraph

instance WebAudio Sound where
  createGraph (Sound s) = do
    graph <- createGraph s
    connectGraph graph
    return graph
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

-- need to create to play the user's entered soundfile. needs to use the correct 'id'
-- (exaclty 'userAudio') to work
audioElement::MonadWidget t m => m ()
audioElement = elDynAttr "audio" attrs (return())
  where attrs = constDyn $ M.fromList $ zip ["id","controls"] ["userAudio","controls"]


-- Creates file input button and a play/pause/scrub interface.
-- Returns a Source and Event that fires whenever the soundfile changes
-- (to be used to re-connect the graph when the file switches)
mediaElement::MonadWidget t m => m (Source,(Event t ()))
mediaElement = el "div" $ do
  let attrs = FileInputConfig (constDyn $ M.fromList $ zip ["id"] ["soundFileInput"])
  file <- fileInput attrs
  let fileChange = (()<$) $ updated $ _fileInput_value file
  audioElement
  performEvent $ fmap liftIO $ fmap (\_ -> createMediaNode) fileChange   -- loads sound player into audio tag everytime the file changes
  return (MediaSource,fileChange)


-- Connects nodes to eachother and last node to destination
connectGraphOnEv :: MonadWidget t m => Event t Sound -> m ()
connectGraphOnEv sound = do 
  performEvent $ fmap liftIO $ fmap (\x->do 
    g <- createGraph x
    connectGraphToDest g
    ) sound
  return ()

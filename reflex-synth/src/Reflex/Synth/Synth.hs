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
    --x <- createMediaNode
    --y <- createAsrEnvelope 0.005 dur 0.005
    --let graph = WebAudioGraph' x (WebAudioGraph y)
    --createGraph graph


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

-- for user to enter their own sound file. must have the right 'id' to work with
-- createUserSoundFileSource
--soundFileInput ::MonadWidget t m => m ()
--soundFileInput = do 
  --let attrs = constDyn M.empty
  --file <- fileInput $ def & _fileInputConfig_attributes ~. attrs
--_fileInputConfig_attributes :: Dynamic t (Map String String)
  --elDynAttr "input" (constDyn $ M.fromList [("value","browse"),("id","soundFileInput"),("type","file"),("accepted","audio/")]) (return ())

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

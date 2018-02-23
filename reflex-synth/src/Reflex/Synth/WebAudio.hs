module Reflex.Synth.WebAudio where

import Reflex.Synth.NodeSpec
import Reflex.Synth.WebAudioNode
import Reflex.Synth.WebAudioGraph
import Reflex.Synth.Sound

class WebAudio a where
  createGraph :: a -> IO WebAudioGraph

instance WebAudio Filter where
  createGraph (NoFilter) = createGain 0 >>= return . WebAudioGraph
  createGraph filt = createBiquadFilter filt >>= return . WebAudioGraph

instance WebAudio Buffer where
  createGraph (File path) = createBufferNode (File path) >>= return . WebAudioGraph

instance WebAudio Node where
  createGraph n = do
    node <- createNode n
    return $ WebAudioGraph $ WebAudioNode n (getJSVal node)

instance WebAudio Source where
  createGraph (NodeSource node dur) =
    case node of
      (MediaNode a) ->do
        v <- createMediaNode a
        let graph = WebAudioGraph v
        createGraph graph
      (Destination) -> error "Destination cannot be a source node"
      (GainNode _) -> error "GainNode cannot be a source node"
      (FilterNode _) -> error "FilterNode cannot be a source node"
      (ScriptProcessorNode _) -> error "ScriptProcessorNode cannot be a source node"
      (CompressorNode _) -> error "CompressorNode cannot be a source node"
      (WaveShaperNode _) -> error "WaveShaperNode cannot be a source node"
      (ConvolverNode _) -> error "ConvolverNode cannot be a source node"
      (DelayNode _) -> error "DelayNode cannot be a source node"
      (BufferNode (LoadedFile soundID _)) -> do
        stopNodeByID soundID
        x <- createNode node
        createGraph (WebAudioGraph x)
      (BufferNode (File s)) ->  case dur of
        (Just dur') -> do
          x <- createBufferNode (File s)
          y <- createAsrEnvelope 0.01 (dur'-0.02) 0.01
          let graph = WebAudioGraph' x (WebAudioGraph y)
          createGraph graph
        (Nothing) -> do
          x <- createBufferNode (File s)
          setBufferNodeLoop x True
          createGraph (WebAudioGraph x)
      otherwise -> case dur of
        (Just dur') -> do
            x <- createNode node
            y <- createAsrEnvelope 0.01 (dur'-0.02) 0.01   --necessary to be percise so disconnectGraphAtTime doesn't clip the sound
            let graph = WebAudioGraph' x (WebAudioGraph y)
            createGraph graph
        (Nothing) -> do
            x <- createNode node
            createGraph (WebAudioGraph x)

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
  createGraph (NoSound) = do
    x <- createSilentNode
    y <- createGain 0
    setAmp 0 y
    let graph = WebAudioGraph' x $ WebAudioGraph y
    connectGraph graph
  createGraph (GainSound s db) = do
    source <- createGraph s
    gain <- createGain db
    let graph = WebAudioGraph'' source (WebAudioGraph gain)
    connectGraph graph
  createGraph (ProcessedSound s effect) = do
    g <- createGraph s
    sp <- createScriptProcessorNode effect
    let graph = WebAudioGraph'' g $ WebAudioGraph sp
    connectGraph graph
  createGraph (WaveShapedSound s w) = do
    g <- createGraph s
    wS <- createWaveShaperNode w
    let graph =  WebAudioGraph'' g $ WebAudioGraph wS
    connectGraph graph
  createGraph (ReverberatedSound s b) = do
    g <- createGraph s
    conv <- createConvolverNode b
    let graph =  WebAudioGraph'' g $ WebAudioGraph conv
    connectGraph graph
  createGraph (OverlappedSound identifier xs) = do
    stopOverlappedSound identifier
    listOfGraphs <- mapM createGraph xs
    arrayOfSources <- toJSArray $ fmap (getJSVal . getFirstNode) listOfGraphs  -- getting all sources in JS array
    F.adddToOverlappedDictionary (toJSString identifier) arrayOfSources -- add 'identifier' mapped to arrayOfSources to a dictionary. So that all the nodes can be stopped when user hit's 'stop' on an overlapped sound.
    gain <- createGain 0 -- 0dB
    let graph = WebAudioGraph''' listOfGraphs gain
    connectGraph graph
  -- createGraph (ScoreSound identifier xs) = do
  --   stopOverlappedSound identifier
  --   listOfGraphs <- mapM (createGraph . snd) xs
  --   arrayOfSources <- toJSArray $ fmap (getJSVal . getFirstNode) listOfGraphs
  --   F.adddToOverlappedDictionary (toJSString identifier) arrayOfSources
  --   gain <- createGain 0
  --   let graph = WebAudioGraph''' listOfGraphs gain
  --   connectGraph graph
  createGraph (DelayedSound s d) = do
    g <- createGraph s
    delay <- createDelayNode d
    connectGraph (WebAudioGraph'' g $ WebAudioGraph delay)
  createGraph (CompressedSound s c) = do
    g <- createGraph s
    comp <- createCompressorNode c
    connectGraph ( WebAudioGraph'' g $ WebAudioGraph comp)

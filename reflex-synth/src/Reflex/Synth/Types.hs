module Reflex.Synth.Types where

--module Reflex.Synth.Types(
--  FilterType (..),
--  NoiseType (..),
--  Node (..),
--  Filter (..),
--  OscillatorType(..),
--  Oscillator(..),
--  Buffer (..),
--  Source (),
--  nodeSource,
--  Sound (..),
--  WebAudioNode (..),
--  WebAudioGraph(..),
--  createGain, createBiquadFilter, createBufferNode, createAsrEnvelope, getJSVal, getFirstNode, getDestination, setFrequency, setFilterQ,
--  setFilterType, setGainAtTime, startNode, connectGraphToDest, startGraph, startFirstNodeconnect, connectGraph, setGain
--  ) where


import GHCJS.DOM.Types(toJSString)
import GHCJS.Marshal
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal)
import qualified Reflex.Synth.Foreign as F
import Control.Monad (mapM)
import Data.Char (toLower)
import qualified GHCJS.Prim as Prim (toJSString)


data FilterType = Peaking | Lowpass | Highpass | Notch | Bandpass | Lowshelf | Highshelf | Allpass deriving (Show,Read,Eq)

data NoiseType = White | Pink | Brownian

data Node =
  SilentNode |
  FilterNode Filter |
  GainNode Double |
  Destination |
  AdditiveNode [Node] |
  OscillatorNode Oscillator |
  BufferNode Buffer |
  MediaNode String |
  CompressorNode  Compressor |
  WaveShaperNode WaveShaper|
  ScriptProcessorNode DSPEffect |
  ConvolverNode Buffer deriving(Read,Show,Eq)

data DSPEffect = DistortAtDb Double deriving (Read, Show, Eq)

data WaveShaper = ClipAt Double deriving (Show, Read, Eq)
data Compressor = Compressor {threshold::Double, knee::Double, ratio::Double, attack::Double, release::Double} deriving (Show, Read, Eq)
data Filter = NoFilter | Filter FilterType Double Double Double deriving (Read,Show,Eq) -- Freq, q, db

data OscillatorType = Sawtooth | Sine | Square deriving (Show, Read,Eq)

data Oscillator = Oscillator OscillatorType Double Double deriving (Read,Show,Eq) --double params are freq and gain (in dB) (respectively)

data PlaybackParam = PlaybackParam{
  startTime::Double,
  endTime::Double,
  loop::Bool
} deriving (Read, Show, Eq)

data Buffer = File String | LoadedFile String PlaybackParam deriving (Read,Show,Eq)

data Source = NodeSource Node (Maybe Double)  deriving (Show, Eq, Read)

data Sound =
  NoSound |
  Sound Source |
  GainSound Sound Double |  -- gain in dB
  FilteredSound Source Filter  |
  CompressedSound Sound Compressor |
  ProcessedSound Sound DSPEffect |
  WaveShapedSound Sound WaveShaper |
  ReverberatedSound Sound Buffer |
  OverlappedSound String [Sound] deriving (Read,Show)  -- String is sort of an unfortunately necessary identifier - so that if playing a sound of an indefinite length (such as a looped buffer) overlapped with other sounds, when you call 'stop' (Read,Show)

soundTwo = OverlappedSound "Test" [Sound (NodeSource (OscillatorNode ( Oscillator Sine 440 (-10))) (Just 2)), Sound ( NodeSource (BufferNode ( File "pinknoise.wav")) (Just 2))]

-- soundOne = OverlappedSound "Test" [Sound $ NodeSource (OscillatorNode $ Oscillator Sine 440 (-10)) (Just 2)]

data WebAudioNode = WebAudioNode Node JSVal | NullAudioNode

-- Might use this eventually...
--type JSNodeRef = (Either JSVal [JSVal],JSVal) -- fst: node that can be 'started' (if there is one), snd: node that can connect to a subsequent node

-- For representing WebAudio Graphs - to be understood as hooking up a sequence of 'nodes' (or ugens)
-- for instance, the web audio graph:
--   oscillator.connect(gain)
--   gain.connect(compressor)
-- would be represented (roughly) as: WebAudioGraph' oscillator (WebAudioGraph' gain (WebAudioGraph compressor))
data WebAudioGraph = WebAudioGraph WebAudioNode |
 WebAudioGraph' WebAudioNode WebAudioGraph |
 WebAudioGraph'' WebAudioGraph WebAudioGraph |
 WebAudioGraph'''  [WebAudioGraph] WebAudioNode -- list of graphs played in parallel, and the last node should be a Gain node to mix them all


createBiquadFilter:: Filter -> IO WebAudioNode
createBiquadFilter (NoFilter) = createGain 0
createBiquadFilter (Filter filtType f q g) = do
  x <- F.createBiquadFilter
  let y = WebAudioNode (FilterNode (Filter filtType f q g)) x
  setFrequency f y
  setFilterQ q y
  setGain g y
  setFilterType filtType y
  return y



createCompressorNode:: Compressor -> IO (WebAudioNode)
createCompressorNode (Compressor a b c d e) = do
  ref <- F.createCompressorNode (pToJSVal a) (pToJSVal b) (pToJSVal c) (pToJSVal d) (pToJSVal e)
  return $ WebAudioNode (CompressorNode $ Compressor a b c d e) ref

createWaveShaperNode:: WaveShaper -> IO WebAudioNode
createWaveShaperNode (ClipAt db) = F.createClipAtWaveShaper (pToJSVal db) >>= return . WebAudioNode (WaveShaperNode $ ClipAt db)

createOscillator :: Oscillator -> IO WebAudioNode
createOscillator (Oscillator t freq db) = do
  osc <- F.createOscillator (Prim.toJSString $ fmap toLower $ show t) (pToJSVal freq) (pToJSVal db)
  -- F.setOscillatorType (Prim.toJSString $ fmap toLower $ show t) osc  -- Web Audio won't accept 'Sine' must be 'sine'
  -- F.setFrequency freq osc
  -- g <- F.createGain
  -- F.setAmp 0 g
  -- F.connect osc g
  -- F.startNode osc
  return (WebAudioNode (OscillatorNode $ Oscillator t freq db) osc)


createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain
  F.setGain g x
  return (WebAudioNode (GainNode g) x)

createBufferNode :: Buffer -> IO WebAudioNode
createBufferNode (File path) = do
  x <- F.createBufferSourceNodeFromURL (Prim.toJSString path)
  return (WebAudioNode (BufferNode $ File path) x)
createBufferNode (LoadedFile inputId (PlaybackParam s e l)) = do
  let s'=  pToJSVal s
  let e'= pToJSVal e
  let l'= pToJSVal l
  x <- F.createBufferSourceNodeFromID (Prim.toJSString inputId) s' e' l'
  return (WebAudioNode (BufferNode $ LoadedFile inputId $ PlaybackParam s e l) x)

createScriptProcessorNode:: DSPEffect -> IO (WebAudioNode)
createScriptProcessorNode (DistortAtDb db) = F.getDistortAtDbFunc (pToJSVal db) >>= F.createScriptProcessorNode >>= return . WebAudioNode (ScriptProcessorNode $ DistortAtDb db)

createAsrEnvelope :: Double -> Double -> Double -> IO WebAudioNode
createAsrEnvelope a s r = do
  now <- F.getCurrentTime
  n <- createGain 0
  setAmp 0.0 n
  linearRampToGainAtTime 0.0 now n
  linearRampToGainAtTime 1.0 (now+a) n
  linearRampToGainAtTime 1.0 (now+a+s) n
  linearRampToGainAtTime 0.0 (now+a+s+r) n
  return n

getJSVal::WebAudioNode -> JSVal
getJSVal (WebAudioNode _ x) = x
getJSVal (NullAudioNode) = error "no JSVal for null audio node"

-- Get the first node in a Graph - usually the first node in a graph function is the 'source' (such as a buffer or oscillator)
-- which needs to be 'started' in the Web Audio API to hear anything.
getFirstNode:: WebAudioGraph -> WebAudioNode
getFirstNode (WebAudioGraph n) = n
getFirstNode (WebAudioGraph' n _) = n
getFirstNode (WebAudioGraph'' n _) = getFirstNode n
getFirstNode (WebAudioGraph''' _ _) = error "getFirstNode:  Cannot get the first node of a mixed signal ( a WebAudioGraph''' )"

-- Gets the lst node in a WebAudioGraph - allows for tacking more ugens/nodes onto the end of a graph
getLastNode:: WebAudioGraph -> WebAudioNode
getLastNode (WebAudioGraph n) = n
getLastNode (WebAudioGraph' _ n) = getLastNode n
getLastNode (WebAudioGraph'' _ n) = getLastNode n


getDestination :: IO WebAudioNode
getDestination = do
  x <- F.getDestination
  return $ WebAudioNode Destination x

connect :: WebAudioNode -> WebAudioNode -> IO (WebAudioGraph)
connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
connect NullAudioNode _ = return (WebAudioGraph NullAudioNode)
connect a NullAudioNode = return (WebAudioGraph a)
connect (WebAudioNode (AdditiveNode xs) r) (WebAudioNode a y) = do
  F.connectAdditiveNode r y
  return $ WebAudioGraph' (WebAudioNode (AdditiveNode xs) r) $ WebAudioGraph (WebAudioNode a y)
connect (WebAudioNode x y) (WebAudioNode (ScriptProcessorNode e) y') = do
  F.spConnect y y'
  return $ WebAudioGraph' (WebAudioNode x y) $ WebAudioGraph (WebAudioNode (ScriptProcessorNode e) y')
connect (WebAudioNode xt x) (WebAudioNode yt y) = do
  F.connect x y
  return $ WebAudioGraph' (WebAudioNode xt x) (WebAudioGraph (WebAudioNode yt y))

disconnect:: WebAudioNode -> WebAudioNode -> IO ()
disconnect (WebAudioNode _ a) (WebAudioNode _ b) = F.disconnect a b

disconnectAll::WebAudioNode -> IO ()
disconnectAll (WebAudioNode _ a) = F.disconnectAll a

disconnectAllAtTime:: WebAudioNode -> Double -> IO ()
disconnectAllAtTime (WebAudioNode _ x) t = F.disconnectAllAtTime x (pToJSVal t)

connectGraph :: WebAudioGraph -> IO (WebAudioGraph)
connectGraph (WebAudioGraph n) = return $ WebAudioGraph n
connectGraph (WebAudioGraph' n (WebAudioGraph n2)) = do
  connect n n2
  return $ (WebAudioGraph' n (WebAudioGraph n2))
connectGraph (WebAudioGraph' n (WebAudioGraph' n2 xs)) = do
  connect n n2
  connectGraph (WebAudioGraph' n2 xs)
  return (WebAudioGraph' n (WebAudioGraph' n2 xs))
connectGraph (WebAudioGraph'' a b) = do
  let aLast = getLastNode a
  let bFirst = getFirstNode b
  connect aLast bFirst
  return (WebAudioGraph'' a b)
connectGraph (WebAudioGraph''' xs n) = do
  sequence $ fmap (\x-> do
    connectGraph x
    connect (getLastNode x) n
    ) xs
  return (WebAudioGraph''' xs n)



setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode (FilterNode _) x) = F.setAmp g x -- filter gain values in Web Audio API are already in dB
setGain g (WebAudioNode (GainNode _) x) = F.setGain g x
setGain _ _ = putStrLn "warning: unmatched pattern in Reflex.Synth.Types.setGain"

setAmp:: Double -> WebAudioNode -> IO ()
-- setAmp a (WebAudioNode (FilterNode _) x) = F.setAmp a x -- this doesn't make sense: filter.gain.value expects db
setAmp a (WebAudioNode (GainNode _) x) = F.setAmp a x
setAmp a (WebAudioNode (OscillatorNode _) x) = F.setOscillatorAmp x a
setAmp _ _ = putStrLn "warning: unmatched pattern in Reflex.Synth.Types.setAmp"

setBufferNodeLoop:: WebAudioNode -> Bool -> IO ()
setBufferNodeLoop (WebAudioNode _ r) b = F.setBufferNodeLoop r (pToJSVal b)


setFrequency:: Double -> WebAudioNode -> IO ()
setFrequency f (WebAudioNode (FilterNode _) x) = F.setFrequency f x
-- @setting freq of other ugens too...

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (FilterNode _) x) = F.setFilterQ q x
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterType :: FilterType -> WebAudioNode-> IO ()
setFilterType filtType (WebAudioNode (FilterNode _) x) = F.setFilterType (Prim.toJSString $ fmap toLower $ show filtType) x
setFilterType _ _ = error "cannot set filter type of a non-filter"

setAmpAtTime:: Double -> Double -> WebAudioNode -> IO ()
setAmpAtTime val t (WebAudioNode _ node) = F.setAmpAtTime val t node
setAmpAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"

linearRampToGainAtTime:: Double -> Double -> WebAudioNode -> IO ()
linearRampToGainAtTime val t (WebAudioNode _ node) = F.linearRampToGainAtTime val t node
linearRampToGainAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"


startNode :: WebAudioNode -> IO ()
startNode (WebAudioNode (AdditiveNode _) r) = F.startNodes r  -- @this may not be the best..
startNode (WebAudioNode (GainNode _) _) = error "Gain node cannot bet 'started' "
startNode (WebAudioNode (MediaNode s) _) = F.playMediaNode (toJSString s) -- if you call 'start' on a MediaBufferNode a js error is thrown by the WAAPI
startNode (WebAudioNode (OscillatorNode (Oscillator _ _ _)) r) = F.startNode r
startNode (WebAudioNode (BufferNode (LoadedFile a (PlaybackParam b c d))) x) = do
  F.playBufferNode (toJSString a) (pToJSVal b) (pToJSVal c) (pToJSVal d) x
startNode (WebAudioNode _ ref) = F.startNode ref
startNode (WebAudioNode (CompressorNode _) _) = error "Compressor node cannot be started"

stopNodeByID:: String -> IO ()
stopNodeByID s = F.stopNodeByID (toJSString s)

stopOverlappedSound:: String -> IO()
stopOverlappedSound = F.stopOverlappedSound .  toJSString


connectGraphToDest:: WebAudioGraph -> IO ()
connectGraphToDest g = do
  let l = getLastNode g
  dest <- getDestination
  connect l dest
  return ()


startFirstNode::WebAudioGraph -> IO()
startFirstNode g = let f = getFirstNode g in startNode f

startGraph :: WebAudioGraph -> IO()
startGraph (WebAudioGraph''' xs n)= do
  dest <- getDestination
  connect n dest
  sequence $ fmap (startNode . getFirstNode) xs
  return ()
startGraph a = do
  let f = getFirstNode a
  let l = getLastNode a
  dest <- getDestination
  connect l dest
  startNode f

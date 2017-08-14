module Reflex.Synth.Types where

import GHCJS.Types (JSVal)
import qualified Reflex.Synth.Foreign as F
import Data.Char (toLower)
import qualified GHCJS.Prim as Prim (toJSString)

data FilterType = Peaking | Lowpass | Highpass | Notch | Bandpass | Lowshelf | Highshelf | Allpass deriving (Show,Read)

data NoiseType = White | Pink | Brownian 

data NodeType = FilterNode FilterType | GainNode | Destination | NoiseNode NoiseType | OscillatorNode Oscillator | BufferNode Buffer | MediaNode

data Filter = NoFilter | Filter FilterType Double Double Double deriving (Read,Show)

data OscillatorType = Sawtooth | Sine | Square deriving (Show, Read)
data Oscillator = Oscillator OscillatorType Double deriving (Read,Show) --The Double is oscillator frequency


data Buffer = File String deriving (Read,Show)


data Source = OscillatorSource Oscillator Double| BufferSource Buffer Double | MediaSource deriving(Read,Show) -- 'Double' is the duration of the source


data Sound = NoSound | FilteredSound Source Filter deriving (Read,Show)

data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode


-- For representing WebAudio Graphs - to be understood as hooking up a sequence of 'nodes' (or ugens)
-- for instance, the web audio graph:
--   oscillator.connect(gain)
--   gain.connect(compressor)
-- would be represented (roughly) as: WebAudioGraph' oscillator (WebAudioGraph' gain (WebAudioGraph compressor))
data WebAudioGraph = WebAudioGraph WebAudioNode | WebAudioGraph' WebAudioNode WebAudioGraph | WebAudioGraph'' WebAudioGraph WebAudioGraph

createOscillator :: Oscillator -> IO WebAudioNode
createOscillator (Oscillator t freq) = do
  osc <- F.createOscillator
  F.setOscillatorType (Prim.toJSString $ fmap toLower $ show t) osc  -- Web Audio won't accept 'Sine' must be 'sine'
  F.setFrequency freq osc
  return (WebAudioNode (OscillatorNode $ Oscillator t freq) osc)

createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain
  setGain g (WebAudioNode GainNode x)
  return (WebAudioNode GainNode x)


createBiquadFilter:: Filter -> IO WebAudioNode
createBiquadFilter (NoFilter) = createGain 1
createBiquadFilter (Filter filtType f q g) = do
  x <- F.createBiquadFilter
  let y = WebAudioNode (FilterNode filtType) x
  setFrequency f y
  setFilterQ q y
  setGain g y
  setFilterType filtType y
  return y


createBufferNode :: Buffer -> IO WebAudioNode
createBufferNode (File path) = do
  x <- F.createAudioBufferSourceNode (Prim.toJSString path)
  return (WebAudioNode (BufferNode $ File path) x)


createMediaNode:: IO WebAudioNode
createMediaNode = do 
  F.loadUserSoundFile
  x <- F.createUserSoundFileNode
  return (WebAudioNode MediaNode x)

createWhiteNoise :: IO WebAudioNode
createWhiteNoise = F.createWhiteNoise >>= return . WebAudioNode (NoiseNode White)

createPinkNoise :: IO WebAudioNode
createPinkNoise = F.createPinkNoise >>= return . WebAudioNode (NoiseNode Pink)

createBrownianNoise :: IO WebAudioNode
createBrownianNoise = F.createBrownianNoise >>= return . WebAudioNode (NoiseNode Brownian)

createAsrEnvelope :: Double -> Double -> Double -> IO WebAudioNode
createAsrEnvelope a s r = do
  now <- F.getCurrentTime
  n <- createGain 1
  setGain 0.0 n
  setGainAtTime 0.0 now n
  setGainAtTime 1.0 (now+a) n
  setGainAtTime 1.0 (now+a+s) n
  setGainAtTime 0.0 (now+a+s+r) n
  return n

-- Get the first node in a Graph - usually the first node in a graph function is the 'source' (such as a buffer or oscillator)
-- which needs to be 'started' in the Web Audio API to hear anything.
getFirstNode:: WebAudioGraph -> WebAudioNode
getFirstNode (WebAudioGraph n) = n
getFirstNode (WebAudioGraph' n _) = n
getFirstNode (WebAudioGraph'' n _) = getFirstNode n

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
connect (WebAudioNode xt x) (WebAudioNode yt y) = do 
  F.connect x y
  return $ WebAudioGraph' (WebAudioNode xt x) (WebAudioGraph (WebAudioNode yt y))

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


setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode (FilterNode _) x) = F.setGain g x
setGain g (WebAudioNode GainNode x) = F.setGain g x

setFrequency:: Double -> WebAudioNode -> IO ()
setFrequency f (WebAudioNode (FilterNode _) x) = F.setFrequency f x
-- @setting freq of other ugens too...

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (FilterNode _) x) = F.setFilterQ q x
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterType :: FilterType -> WebAudioNode-> IO ()
setFilterType filtType (WebAudioNode (FilterNode _) x) = F.setFilterType (Prim.toJSString $ fmap toLower $ show filtType) x
setFilterType _ _ = error "cannot set filter type of a non-filter"

setGainAtTime:: Double -> Double -> WebAudioNode -> IO ()
setGainAtTime val t (WebAudioNode _ node) = F.setGainAtTime val t node
setGainAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"

startNode :: WebAudioNode -> IO ()
startNode (WebAudioNode MediaNode _) = return ()  -- if you call 'start' on a MediaBufferNode a js error is thrown by the WAAPI
startNode (WebAudioNode _ ref) = F.startNode ref
startNode _ = return ()

startGraph :: WebAudioGraph -> IO()
startGraph a = do
  let f = getFirstNode a
  let l = getLastNode a
  dest <- getDestination
  connect l dest
  startNode f

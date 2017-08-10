module Reflex.Synth.Types where

import GHCJS.Types (JSVal)
import qualified Reflex.Synth.Foreign as F

data FilterType = Peaking | Lowpass

data NoiseType = White | Pink | Brownian

data NodeType = FilterNode FilterType | GainNode | Destination | NoiseNode NoiseType | OscillatorNode Oscillator

--data Filter = NoFilter |
--              PeakingFilter Double Double Double -- Frequency Q Gain

data Filter = NoFilter | Filter FilterType Double Double Double

type Duration = Double

data Source = PinkNoiseSource Duration | OscillatorSource Oscillator Duration

data Sound = NoSynth | FilteredSound Source Filter

--data Oscillator = Saw Double | Sine Double | Square Double

data Oscillator = Oscillator OscillatorType Double --The Double is oscillator frequency

data OscillatorType = Sawtooth | Sine | Square deriving (Show)

data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode



-- JSVal is a reference to the node furthest down the 'chain' of connected nodes
-- for instance the Web Audio code:
--    oscillator.connect(compressor)
--    compressor.connect(filter)
--    filter.connect(ac.destination)
-- 'filter' references the node furthest down the chain and
-- the WebAudioGraph would look lik:  WebAudioGraph (oscillator) $ WebAudioGraph compressor $ WebAudioGraph filter [filterJSVal]
--data WebAudioGraph = WebAudioGraph WebAudioNode JSVal | WebAudioGraph' WebAudioNode WebAudioGraph
data WebAudioGraph = WebAudioGraph WebAudioNode | WebAudioGraph' WebAudioNode WebAudioGraph | WebAudioGraph'' WebAudioGraph WebAudioGraph


createSaw :: IO WebAudioNode
createSaw = do
  osc <- F.createOscillator
  F.setOscillatorSaw osc
  F.setOscillatorFrequency osc 220
  return (WebAudioNode (OscillatorNode $ Oscillator Sawtooth 220) osc)



getFirstNode:: WebAudioGraph -> WebAudioNode
getFirstNode (WebAudioGraph n) = n
getFirstNode (WebAudioGraph' n _) = n
getFirstNode (WebAudioGraph'' n _) = getFirstNode n

getLastNode:: WebAudioGraph -> WebAudioNode
getLastNode (WebAudioGraph n) = n
getLastNode (WebAudioGraph' _ n) = getLastNode n
getLastNode (WebAudioGraph'' _ n) = getLastNode n

--WebAudioGraph' (Buffer) (WebAudioGraph (filter)) 
--WebAudioGraph' (oscillator) (WebAudioGraph (compressor))
--data WebAudioGraph = WebAudioGraph Source Effect | WebAudioGraph' WebAudioGraph Effect
--data Source = Oscillator | Noise | Buffer | NullAudioNode
--data Effect = Filter | Compressor 


getDestination :: IO WebAudioNode
getDestination = do
  x <- F.getDestination 
  return $ WebAudioNode Destination x

--connect :: WebAudioNode -> WebAudioNode -> IO ()
--connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
--connect NullAudioNode _ = return ()
--connect _ NullAudioNode = return ()
--connect (WebAudioNode _ x) (WebAudioNode yt y) = F.connect x y

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

--connectGraph' :: WebAudioGraph -> IO (WebAudioNode)
--connectGraph' (WebAudioGraph n) = do return n
--connectGraph' (WebAudioGraph' n (WebAudioGraph n2)) = do 
--  connect n n2
--  return n2
--connectGraph' (WebAudioGraph' n (WebAudioGraph' n2 xs)) = do
--  connect n n2
--  endNode <- connectGraph' (WebAudioGraph' n2 xs)
--  return endNode

--connect (WebAudioNode _ x) (WebAudioNode yt y) = do 
--  F.connect x y
--  return (WebAudioGraph ())

setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode GainNode x) = F.setGain g x
setGain _ _ = error "can't set gain value of node not of type Gain"

createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain
  setGain g (WebAudioNode GainNode x)
  return (WebAudioNode GainNode x)

--createBiquadFilter :: IO WebAudioNode
--createBiquadFilter = F.createBiquadFilter >>= return . WebAudioNode (FilterNode Peaking) 

--createPeakingFilter :: Double -> Double -> Double -> IO WebAudioNode
--createPeakingFilter f q g = do
--  x <- F.createBiquadFilter
--  let y = WebAudioNode (FilterNode Peaking) x
--  setFilterF f y
--  setFilterQ q y
--  setFilterGain g y
--  return y


createBiquadFilter:: Filter -> IO WebAudioNode
createBiquadFilter (NoFilter) = createGain 1
createBiquadFilter (Filter filtType f q g) = do
  x <- F.createBiquadFilter
  let y = WebAudioNode (FilterNode filtType) x
  setFilterF f y
  setFilterQ q y
  setFilterGain g y
  setFilterType filtType y 
  return y

--createFilter:: FilterType -> Double -> Double -> Double -> IO(WebAudioNode)
--createFilter filtType f q g = do
--  x <- F.createBiquadFilter
--  let y = WebAudioNode (FilterNode filtType) x
--  setFilterF f y
--  setFilterQ q y
--  setFilterGain g y
--  setFilterType filtType y 
--  return y


setFilterF :: Double -> WebAudioNode -> IO ()
setFilterF f (WebAudioNode (FilterNode _) x) = do
  F.setF x f
  return ()
setFilterF _ _ = error "can't setFilterF on non-filter"

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (FilterNode _) x) = do
  F.setQ x q
  return ()
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterGain :: Double -> WebAudioNode -> IO ()
setFilterGain g (WebAudioNode (FilterNode _) x) = do
  F.setFilterGain x g
  return ()
setFilterGain _ _ = error "can't setFilterGain on non-filter"

setFilterType:: FilterType -> WebAudioNode -> IO ()
setFilterType (Peaking) (WebAudioNode (FilterNode _) x) = F.setFilterPeaking x
setFilterType (Lowpass) (WebAudioNode (FilterNode _) x) = F.setFilterLowpass x
--  @Finish for rest of filter types
setFilterType a _ = error "can't set filter type of a non-filter node"

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


setGainAtTime:: Double -> Double -> WebAudioNode -> IO ()
setGainAtTime val t (WebAudioNode _ node) = F.setGainAtTime val t node
setGainAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"

startNode :: WebAudioNode -> IO ()
startNode (WebAudioNode _ ref) = F.startNode ref
startNode _ = return ()

startGraph :: WebAudioGraph -> IO()
startGraph a = do
  let f = getFirstNode a
  let l = getLastNode a
  dest <- getDestination
  connect l dest
  startNode f



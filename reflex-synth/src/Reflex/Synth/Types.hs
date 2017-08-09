module Reflex.Synth.Types where

import GHCJS.Types (JSVal)
import qualified Reflex.Synth.Foreign as F

data FilterType = Peaking

data NoiseType = White | Pink | Brownian

data NodeType = Filter FilterType | Gain | Destination | Noise NoiseType

data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode

-- JSVal is a reference to the node furthest down the 'chain' of connected nodes
-- for instance the Web Audio code:
--    oscillator.connect(compressor)
--    compressor.connect(filter)
--    filter.connect(ac.destination)
-- 'filter' references the node furthest down the chain and
-- the WebAudioGraph would look lik:  WebAudioGraph (oscillator) $ WebAudioGraph compressor $ WebAudioGraph filter [filterJSVal]
--data WebAudioGraph = WebAudioGraph WebAudioNode JSVal | WebAudioGraph' WebAudioNode WebAudioGraph
data WebAudioGraph = WebAudioGraph WebAudioNode | WebAudioGraph' WebAudioNode WebAudioGraph


getDestination :: IO WebAudioNode
getDestination = do
  x <- F.getDestination 
  return $ WebAudioNode Destination x

connect :: WebAudioNode -> WebAudioNode -> IO ()
connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
connect NullAudioNode _ = return ()
connect _ NullAudioNode = return ()
connect (WebAudioNode _ x) (WebAudioNode yt y) = F.connect x y

connectGraph :: WebAudioGraph -> IO()
connectGraph (WebAudioGraph n) = do return ()
connectGraph (WebAudioGraph' n (WebAudioGraph n2)) = connect n n2
connectGraph (WebAudioGraph' n (WebAudioGraph' n2 xs)) = do
  connect n n2
  connectGraph (WebAudioGraph' n2 xs)

connectGraph' :: WebAudioGraph -> IO (WebAudioNode)
connectGraph' (WebAudioGraph n) = do return n
connectGraph' (WebAudioGraph' n (WebAudioGraph n2)) = do 
  connect n n2
  return n2
connectGraph' (WebAudioGraph' n (WebAudioGraph' n2 xs)) = do
  connect n n2
  endNode <- connectGraph' (WebAudioGraph' n2 xs)
  return endNode

--connect (WebAudioNode _ x) (WebAudioNode yt y) = do 
--  F.connect x y
--  return (WebAudioGraph ())

setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode Gain x) = F.setGain g x
setGain _ _ = error "can't set gain value of node not of type Gain"

createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain
  setGain g (WebAudioNode Gain x)
  return (WebAudioNode Gain x)

createBiquadFilter :: IO WebAudioNode
createBiquadFilter = F.createBiquadFilter >>= return . WebAudioNode (Filter Peaking) 

createPeakingFilter :: Double -> Double -> Double -> IO WebAudioNode
createPeakingFilter f q g = do
  x <- F.createBiquadFilter
  let y = WebAudioNode (Filter Peaking) x
  setFilterF f y
  setFilterQ q y
  setFilterGain g y
  return y

setFilterF :: Double -> WebAudioNode -> IO ()
setFilterF f (WebAudioNode (Filter _) x) = do
  F.setF x f
  return ()
setFilterF _ _ = error "can't setFilterF on non-filter"

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (Filter _) x) = do
  F.setQ x q
  return ()
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterGain :: Double -> WebAudioNode -> IO ()
setFilterGain g (WebAudioNode (Filter _) x) = do
  F.setFilterGain x g
  return ()
setFilterGain _ _ = error "can't setFilterGain on non-filter"

createWhiteNoise :: IO WebAudioNode
createWhiteNoise = F.createWhiteNoise >>= return . WebAudioNode (Noise White)

createPinkNoise :: IO WebAudioNode
createPinkNoise = F.createPinkNoise >>= return . WebAudioNode (Noise Pink)

createBrownianNoise :: IO WebAudioNode
createBrownianNoise = F.createBrownianNoise >>= return . WebAudioNode (Noise Brownian)

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

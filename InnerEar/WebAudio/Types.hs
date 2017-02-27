module InnerEar.WebAudio.Types where

import GHCJS.Types (JSVal)
import qualified InnerAudio.WebAudio.Foreign as F

data FilterType = Peaking

data NoiseType = White | Pink | Brownian

data NodeType = Filter FilterType | Gain | Destination | Noise NoiseType

data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode

getDestination :: IO WebAudioNode
getDestination = F.getDestination >>= return $ WebAudioNode Destination

connect :: WebAudioNode -> WebAudioNode -> IO ()
connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
connect NullAudioNode _ = return ()
connect _ NullAudioNode = return ()
connect (WebAudioNode _ x) (WebAudioNode yt y) = F.connect x y

setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode Gain x) = F.setGain g x
setGain _ _ = error "can't set gain value of node not of type Gain"

createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain >>= WebAudioNode Gain
  setGain g x
  return x

createBiquadFilter :: IO WebAudioNode
createBiquadFilter = F.createBiQuadFilter >>= return $ WebAudioNode Filter

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
  F.setF f x
  return ()
setFilterF _ _ = error "can't setFilterF on non-filter"

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (Filter _) x) = do
  F.setQ q x
  return ()
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterGain :: Double -> WebAudioNode -> IO ()
setFilterGain g (WebAudioNode (Filter _) x) = do
  F.setFilterGain g x
  return ()
setFilterGain _ _ = error "can't setFilterGain on non-filter"

createWhiteNoise :: IO WebAudioNode
createWhiteNoise = F.createWhiteNoise >>= return $ WebAudioNode (Noise White)

createPinkNoise :: IO WebAudioNode
createPinkNoise = F.createPinkNoise >>= return $ WebAudioNode (Noise Pink)

createBrownianNoise :: IO WebAudioNode
createBrownianNoise = F.createBrownianNoise >>= return $ WebAudioNode (Noise Brownian)

createAsrEnvelope :: Double -> Double -> Double -> IO WebAudioNode
createAsrEnvelope a s r = do
  now <- getCurrentTime
  n <- createGainNode'
  setGain 0.0 n
  setGainAtTime 0.0 now n
  setGainAtTime 1.0 (now+a) n
  setGainAtTime 1.0 (now+a+s) n
  setGainAtTime 0.0 (now+a+s+r) n
  return n

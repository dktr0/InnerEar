module InnerEar.WebAudio.Types where

import GHCJS.Types (JSVal)
import qualified InnerAudio.WebAudio.Foreign as F

data NodeType = Filter | Gain | Destination

data WebAudioNode = WebAudioNode NodeType JSVal

getDestination :: IO WebAudioNode
getDestination = F.getDestination >>= return $ WebAudioNode Destination x

connect :: WebAudioNode -> WebAudioNode -> IO ()
connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
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

data FilterType = Peaking

setFilterType :: FilterType -> WebAudioNode -> IO ()
setFilterType Peaking (WebAudioNode Filter x) = F.setType "peaking" x
setFilterType _ _ = error "can't setFilterType on non-filter"

setFilterF :: Double -> WebAudioNode -> IO ()
setFilterF f (WebAudioNode Filter x) = F.setF f x
setFilterF _ _ = error "can't setFilterF on non-filter"

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode Filter x) = F.setQ q x
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterGain :: Double -> WebAudioNode -> IO ()
setFilterGain g (WebAudioNode Filter x) = F.setFilterGain g x
setFilterGain g (WebAudioNode x) = error "FFI call goes here"

createFilter :: FilterType -> Double -> Double -> Double -> IO WebAudioNode
createFilter t f q g = do
  x <- createBiquadFilter
  setFilterType t x
  setFilterF f x
  setFilterQ q x
  setFilterGain g x
  return x

createPinkNoiseNode :: IO WebAudioNode
createPinkNoiseNode = error "FFI call goes here"

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

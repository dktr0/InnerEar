module InnerEar.WebAudio where

data WebAudioNode = WebAudioNode JSVal

connectNodes :: WebAudioNode -> WebAudioNode -> IO WebAudioNode
connectNodes (WebAudioNode x) (WebAudioNode y) = error "FFI call goes here"

connectToDestination :: WebAudioNode -> IO ()
connectToDestination (WebAudioNode x) = error "FFI call goes here"

setGain :: Double -> WebAudioNode -> IO WebAudioNode
setGain g (WebAudioNode x) = error "FFI call goes here"

createGainNode' :: IO WebAudioNode
createGainNode' = error "FFI call goes here"

createGainNode :: Double -> IO WebAudioNode
createGainNode g = createGainNode' >>= setGainValue g

createFilter' :: IO WebAudioNode
createFilter' = error "FFI call goes here"

data FilterType = Peaking

setFilterType :: FilterType -> WebAudioNode -> IO WebAudioNode
setFilterType Peaking (WebAudioNode x) = error "FFI call goes here"

setFilterF :: Double -> WebAudioNode -> IO WebAudioNode
setFilterF f (WebAudioNode x) = error "FFI call goes here"

setFilterQ :: Double -> WebAudioNode -> IO WebAudioNode
setFilterQ q (WebAudioNode x) = error "FFI call goes here"

setFilterGain :: Double -> WebAudioNode -> IO WebAudioNode
setFilterGain g (WebAudioNode x) = error "FFI call goes here"

createFilter :: FilterType -> Double -> Double -> Double -> IO WebAudioNode
createFilter t f q g = createFilter' >>= setFilterType t >>= setFilterF f >>= setFilterQ q >>= setFilterGain g

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

module Reflex.Synth.AudioRoutingGraph where

import Reflex.Synth.Spec
import Reflex.Synth.Graph
import GHCJS.Types
import GHCJS.Marshal.Pure
import qualified Data.Map as Map

newtype WebAudioContext = WebAudioContext JSVal

newtype AudioParam = AudioParam JSVal

newtype AudioBuffer = AudioBuffer JSVal

newtype Float32Array = Float32Array JSVal

foreign import javascript safe 
  "new (window.AudioContext || window.webkitAudioContext)()"
  js_newAudioContext :: IO WebAudioContext
  
foreign import javascript safe
  "$1.currentTime"
  js_currentTime :: WebAudioContext -> IO Double
  -- time in seconds
  
foreign import javascript safe
  "$1.sampleRate"
  js_sampleRate :: WebAudioContext -> IO Float
  -- ctx -> sampleRate (frames/sec)
  

foreign import javascript safe
  "$1.createOscillator()"
  js_createOscillator :: WebAudioContext -> IO JSVal
  
foreign import javascript safe
  "$1.createBufferSource()"
  js_createBufferSource :: WebAudioContext -> IO JSVal
  
foreign import javascript safe
  "$1.createBiquadFilter()"
  js_createBiquadFilter :: WebAudioContext -> IO JSVal
  
foreign import javascript safe
  "$1.createDelay($2)"
  js_createDelay :: WebAudioContext -> Double -> IO JSVal
  
foreign import javascript safe
  "$1.createDynamicsCompressor()"
  js_createDynamicsCompressor :: WebAudioContext -> IO JSVal
  
foreign import javascript safe
  "$1.createGain()"
  js_createGain :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createWaveShaper()"
  js_createWaveShaper :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.destination"
  js_destination :: WebAudioContext -> IO JSVal


foreign import javascript safe
  "$1[$2] = $3;"
  js_setField :: JSVal -> JSString -> JSVal -> IO ()
  
foreign import javascript safe
  "$1[$2]"
  js_audioParam :: JSVal -> JSString -> IO AudioParam
  
foreign import javascript safe
  "$1.setValueAtTime($2, $3.currentTime);"
  js_setParamValue :: AudioParam -> Double -> WebAudioContext -> IO ()
  
foreign import javascript safe
  "$1.setValueAtTime($2, $3);"
  js_setParamValueAtTime :: AudioParam -> Double -> Double -> IO ()
  
foreign import javascript safe
  "$1.linearRampToValueAtTime($2, $3);"
  js_linearRampToParamValueAtTime :: AudioParam -> Double -> Double -> IO ()
  
foreign import javascript safe
  "$1.exponentialRampToValueAtTime($2, $3);"
  js_exponentialRampToParamValueAtTime :: AudioParam -> Double -> Double -> IO ()
  
foreign import javascript safe
  "$1.setValueCurveAtTime($2, $3, $4);"
  js_setParamValueCurveAtTime :: AudioParam -> Float32Array -> Double -> Double -> IO ()
  

foreign import javascript safe
  "$4.createAudioBuffer($1, $2, $3)"
  js_createAudioBuffer :: Int -> Int -> Float -> WebAudioContext -> IO AudioBuffer
  -- numChannels -> length (in samples) -> sampleRate (frames/sec) -> ctx -> buffer

foreign import javascript safe
  "$1.getChannelData($2)"
  js_channelData :: AudioBuffer -> Int -> IO Float32Array
  -- buffer -> channel (0 indexed) -> data
  
foreign import javascript safe
  "if (Float32Array.prototype.fill !== void 0) $1.fill($2); \n\
  \else for (var i = 0, len = a.length; i < len; i++) $1[i] = $2;"
  js_typedArrayFill :: Float32Array -> Float -> IO ()

foreign import javascript safe
  "new Float32Array($1)"
  js_typedArrayFromArray :: JSVal -> IO Float32Array
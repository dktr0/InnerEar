module Reflex.Synth.AudioRoutingGraph (
) where

import Reflex.Synth.Spec
import Reflex.Synth.Graph
import GHCJS.Types
import GHCJS.Marshal.Pure
import qualified Data.Map as Map

newtype WebAudioContext = WebAudioContext JSVal

newtype AudioBuffer = AudioBuffer JSVal

newtype Float32Array = Float32Array JSVal

newtype CAudioNode a = CAudioNode a

newtype CAudioSourceNode a = CAudioSourceNode a

newtype CBufferSourceNode a = CBufferSourceNode a
type BufferSourceNode = CAudioNode (CAudioSourceNode (CBufferSourceNode JSVal))

newtype OscillatorNode = OscillatorNode (AudioSourceNode

data BiquadFilterNode
data DelayNode
data DynamicsCompressorNode

data DestinationNode

data ConnectionTarget
data ConnectionSource

class AudioSourceNode a where
  start :: a -> IO ()
  start a = return () -- js_start
  
  stop :: a -> IO ()
  stop a = return () -- js_stop
  
class AudioSourceSinkNode a
class AudioSinkNode a
class AudioParam a

class ConnectionTarget a

instance AudioSourceSinkNode a => ConnectionTarget a
instance AudioSinkNode a => ConnectionTarget a
instance AudioParam a => ConnectionTarget a

class ConnectionSource a

instance AudioSourceNode BufferSourceNode
instance AudioSourceSinkNode BufferSourceNode
instance AudioSinkNode BufferSourceNode

connect :: (ConnectionSource a, ConnectionTarget b) => a -> b -> IO ()
connect a b = return ()

foreign import javascript safe 
  "new (window.AudioContext || window.webkitAudioContext)()"
  js_newAudioContext :: IO WebAudioContext
  
foreign import javascript safe
  "$1.currentTime"
  js_currentTime :: WebAudioContext -> IO Double
  
foreign import javascript safe
  "$1.sampleRate"
  js_sampleRate :: WebAudioContext -> IO Float
  -- ctx -> sampleRate (frames/sec)
  
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

-- source nodes
foreign import javascript safe
  "$1.createBufferSource()"
  js_newBufferSource :: WebAudioContext -> IO (AudioSource BufferSourceNode)
foreign import javascript safe
  "$1.loop = $2"
  js_loop :: BufferSourceNode -> Bool -> IO ()

foreign import javascript safe
  "$1.createOscillator()"
  js_newOscillator :: WebAudioContext -> IO (AudioNode OscillatorNode)

-- intermediate nodes
-- Q: 1, detune: 0, frequency: 350, gain: 0
foreign import javascript safe
  "$1.createBiquadFilter()"
  js_newBiquadFilter :: WebAudioContext -> IO (AudioNode BiquadFilterNode)
  
foreign import javascript safe
  "$1.createDelay()"
  js_newDelay :: WebAudioContext -> IO (AudioNode DelayNode)

foreign import javascript safe
  "$1.createDynamicsCompressor()"
  js_newDynamicsCompressor :: WebAudioContext -> IO (AudioNode DynamicsCompressorNode)

-- sink nodes
foreign import javascript safe
  "$1.destination"
  js_destination :: WebAudioContext -> IO (AudioNode DestinationNode)


foreign import javascript safe
  "$1.connect($2)"
  js_connect :: (AudioSource a) => a -> AudioNode b -> IO ()

newSilentNode :: WebAudioContext -> IO BufferSourceNode
newSilentNode ctx = do
  sampleRate <- js_sampleRate ctx
  buffer <- js_createAudioBuffer 1 (ceiling $ sampleRate * 10) sampleRate ctx
  channelData <- js_channelData buffer 0
  js_typedArrayFill channelData 0
  js_newBufferSource ctx
  --TODO set source to buffer

type AudioNodeConstructor a = WebAudioContext -> IO (AudioNode a)

compile :: Synth a -> IO WebAudioContext
compile _ = undefined

newSourceNodeFromSpec :: SourceNodeSpec -> WebAudioContext -> IO JSVal
newSourceNodeFromSpec Silent = newSilentNode
--newSourceNodeFromSpec (Oscillator oscType freq) = js_newOscillator (pToJSVal oscType) (inHz freq)

newSinkNodeFromSpec :: SinkNodeSpec -> WebAudioContext -> IO (AudioNode a)
newSinkNodeFromSpec Destination = js_destination


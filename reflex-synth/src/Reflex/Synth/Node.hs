module Reflex.Synth.Node (
  WebAudioContext,
  Node(..)
) where

import Reflex.Synth.AudioRoutingGraph
import GHCJS.Marshal.Pure

data Node
  -- Source nodes
  = AudioBufferSourceNode { jsval :: JSVal }
  | OscillatorNode { jsval :: JSVal }
  -- SourceSink nodes
  | BiquadFilterNode { jsval :: JSVal }
  | DelayNode { jsval :: JSVal }
  | DynamicsCompressorNode { jsval :: JSVal }
  | GainNode { jsval :: JSVal }
  | WaveShaperNode { jsval :: JSVal }
  | ScriptProcessorNode { jsval :: JSVal }
  -- Sink nodes
  | DestinationNode { jsval :: JSVal }

createAudioContext :: IO WebAudioContext
createAudioContext = js_newAudioContext

setFrequencyHz :: (FrequencyInHz f) => JSVal -> f -> WebAudioContext -> IO ()
setFrequencyHz node f = js_setParamValue (js_audioParam node "frequency") $ inHz f

setGainDb :: (AmplitudeInDb g) => JSVal -> g -> WebAudioContext -> IO ()
setGainDb node g = js_setParamValue (js_audioParam node "gain") $ inDb g

setQ :: JSVal -> Double -> WebAudioContext -> IO ()
setQ node q = js_setParamValue (js_audioParam node "Q") q

instantiateSourceNode :: SourceNodeSpec -> WebAudioContext -> IO Node
instantiateSourceNode SilentNode ctx = do
  sampleRate <- js_sampleRate ctx
  buffer <- js_createAudioBuffer 1 (ceiling $ sampleRate * 10) sampleRate ctx
  channelData <- js_channelData buffer 0
  js_typedArrayFill channelData 0
  src <- js_createBufferSource ctx
  js_setField src "buffer" buffer
  js_setField src "loop" True
  return $ AudioBufferSourceNode src
instantiateSourceNode (Oscillator t f) ctx = do
  osc <- js_createOscillator
  js_setField osc "type" $ pToJSVal t
  setFrequencyHz osc f ctx
  return $ OscillatorNode osc

instantiateSourceSinkNode :: SourceSinkNodeSpec -> WebAudioContext -> IO Node
instantiateSourceSinkNode (Filter spec) ctx = do
  filter <- js_createBiquadFilter ctx
  configureBiquadFilterNode spec filter ctx
  return $ BiquadFilterNode filter
instantiateSourceSinkNode (Delay t) = do
  delay <- js_createDelay $ inSec t -- createDelay needs a maxDelayTime
  js_setParamValue delay "delayTime" (inSec t) ctx
  return $ DelayNode delay
instantiateSourceSinkNode (Compressor thr kne rat red att rel) ctx = do
  comp <- js_createDynamicsCompressor ctx
  let setProp n v = js_setParamValue (js_audioParam comp n) v ctx in
    setProp "threshold" $ inDb thr
    setProp "knee" $ inDb kne
    setProp "ratio" $ inDb rat
    js_setField comp "reduction" $ inDb red
    setProp "attack" $ inSec att
    setProp "release" $ inSec rel
    return $ DynamicsCompressorNode comp
instantiateSourceSinkNode (Gain g) ctx = do
  gain <- js_createGain ctx
  js_setParamValue (js_audioParam gain "gain") (inAmp g) ctx
  return $ GainNode gain
instantiateSourceSinkNode (WaveShaper curve oversample) ctx = do
  shaper <- js_createWaveShaper ctx
  curveArray <- js_typedArrayFromArray $ toJSArray $ fmap pToJSVal curve
  js_setField shaper "curve" curveArray
  js_setField shaper "oversample" $ pToJSVal oversample
  return $ WaveShaperNode shaper

configureBiquadFilterNode :: FilterSpec -> JSVal -> WebAudioContext -> IO ()
configureBiquadFilterNode (LowPass f q) node ctx =
  js_setField node "type" "lowpass" >> setFrequency node f ctx >> setQ node q ctx
configureBiquadFilterNode (HighPass f q) node ctx =
  js_setField node "type" "highpass" >> setFrequency node f ctx >> setQ node q ctx
configureBiquadFilterNode (BandPass f q) node ctx =
  js_setField node "type" "bandpass" >> setFrequency node f ctx >> setQ node q ctx
configureBiquadFilterNode (LowShelf f g) node ctx =
  js_setField node "type" "lowshelf" >> setFrequency node f ctx >> setGain node g ctx
configureBiquadFilterNode (HighShelf f g) node ctx =
  js_setField node "type" "highshelf" >> setFrequency node f ctx >> setGain node g ctx
configureBiquadFilterNode (Peaking f q g) node ctx =
  js_setField node "type" "peaking" >> setFrequency node f ctx >> setQ node q ctx >> setGain node g ctx
configureBiquadFilterNode (Notch f q) node ctx =
  js_setField node "type" "notch" >> setFrequency node f ctx >> setQ node q ctx
configureBiquadFilterNode (AllPass f q) node ctx =
  js_setField node "type" "allpass" >> setFrequency node f ctx >> setQ node q ctx

instantiateSinkNode :: SinkNodeSpec -> WebAudioContext -> IO Node
instantiateSinkNode Destination ctx = js_destination ctx >>= return . DestinationNode 






getDestination :: IO Node
getDestination = getDestination_ >>= return . Destination

getAudioBufferSourceNode :: ? -> IO Node
getAudioBufferSourceNode x = getAudioBufferSourceNode_ x >>= return . AudioBufferSourceNode

-- type Amplitude = Gain

getGainNode :: Gain -> IO Node
getGainNode g = getGainNode_ g >>= return . GainNode

connect :: Node -> Node -> IO ()
connect (Destination _) _ = error "Destination can't be source"
connect _ (AudioBufferSourceNode _) = error "AudioBufferSourceNode can't be sink"
connect x y = connect_ (nodeJSVal x) (nodeJSVal y)

disconnect :: Node -> Node -> IO ()
disconnect (Destination _) _ = error "Destination can't be source"
disconnect _ (AudioBufferSourceNode _) = error "AudioBufferSourceNode can't be sink"
disconnect x y = disconnect_ (nodeJSVal x) (nodeJSVal y)

disconnectAll :: Node -> IO ()
disconnectAll (Destination _) = return ()
disconnectAll x = disconnectAll_ (nodeJSVal x)

start :: Node -> IO ()
start (AudioBufferSourceNode x) = start_ x
start _ = return ()

stop :: Node -> IO ()
stop (AudioBufferSourceNode x) = stop_ x
stop _ = return ()

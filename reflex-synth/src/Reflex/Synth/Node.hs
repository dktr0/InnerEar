module Reflex.Synth.Node (
  WebAudioContext,
  Node(..),
  instantiateSourceNode,
  instantiateSourceSinkNode,
  instantiateSinkNode,
  connect,
  disconnect,
  disconnectAll,
  start,
  stop
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
  | AudioParamNode { jsval :: JSVal }
  
instance Show Node where
  show (AudioBufferSourceNode _) = "AudioBufferSourceNode"
  show (OscillatorNode _) = "OscillatorNode"
  show (BiquadFilterNode _) = "BiquadFilterNode"
  show (DelayNode _) = "DelayNode"
  show (DynamicsCompressorNode _) = "DynamicsCompressorNode"
  show (GainNode _) = "GainNode"
  show (WaveShaperNode _) = "WaveShaperNode"
  show (ScriptProcessorNode _) = "ScriptProcessorNode"
  show (DestinationNode _) = "DestinationNode"
  
isSourceNode :: Node -> Bool
isSourceNode (AudioBufferSourceNode _) = True
isSourceNode (OscillatorNode _) = True
isSourceNode _ = False

isSinkNode :: Node -> Bool
isSinkNode (DestinationNode _) = True
isSinkNode (AudioParamNode _) = True
isSinkNode _ = False

createAudioContext :: IO WebAudioContext
createAudioContext = js_newAudioContext

getCurrentTime :: WebAudioContext -> IO Time
getCurrentTime ctx = js_currentTime ctx >>= return . Sec

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

connect :: Node -> Node -> IO ()
connect from to
  | isSink from = error $ (show from) ++ " can't be connect source."
  | isSource to = error $ (show to) ++ " can't be connect target." 
  | otherwise   = js_connect (jsval from) (jsval to)

disconnect :: Node -> Node -> IO ()
disconnect from to
  | isSink from = error $ (show from) ++ " can't be disconnect source."
  | isSource to = error $ (show to) ++ " can't be disconnect target." 
  | otherwise   = js_disconnect (jsval from) (jsval into)

disconnectAll :: Node -> IO ()
disconnectAll x
  | isSink x = return ()
  | otherwise  = js_disconnectAll (jsval x)

start :: Node -> IO ()
start x
  | isSource x = js_start (jsval x)
  | otherwise  = return ()

stop :: Node -> IO ()
stop x
  | isSource x = js_stop (jsval x)
  | otherwise  = return ()

setParamValueAtTime :: (TimeInSec t) => Node -> String -> Double -> t -> IO ()
setParamValueAtTime node paramName value time = do
  param <- js_audioParam (jsval node) $ pToJSVal paramName
  js_setParamValueAtTime param value (pToJSVal $ inSec t)

linearRampToParamValueAtTime :: (TimeInSec t) => Node -> String -> Double -> t -> IO ()
linearRampToPAramValueAtTime node param value time = do
  param <- js_audioPAram (jsval node)

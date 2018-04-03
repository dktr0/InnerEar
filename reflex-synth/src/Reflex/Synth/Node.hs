module Reflex.Synth.Node (
  WebAudioContext,
  Node(..),
  isSourceNode,
  isSinkNode,
  globalAudioContext,
  getCurrentTime,
  instantiateSourceNode,
  instantiateSourceSinkNode,
  instantiateSinkNode,
  audioParamNode,
  connect,
  disconnect,
  disconnectAll,
  start,
  stop,
  onended,
  setParamValueAtTime,
  linearRampToParamValueAtTime,
  exponentialRampToParamValueAtTime,
  setParamValueCurveAtTime
) where

import Reflex.Synth.AudioRoutingGraph
import Reflex.Synth.Spec
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback(asyncCallback1, releaseCallback)
import GHCJS.Prim(JSVal, toJSArray, toJSString)

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

globalAudioContext :: IO WebAudioContext
globalAudioContext = js_setupGlobalAudioContext

getCurrentTime :: WebAudioContext -> IO Time
getCurrentTime ctx = js_currentTime ctx >>= return . Sec

setFrequencyHz :: JSVal -> Frequency -> WebAudioContext -> IO ()
setFrequencyHz node f = js_setParamValue (js_audioParam node $ toJSString "frequency") $ inHz f

setGainDb :: JSVal -> Gain -> WebAudioContext -> IO ()
setGainDb node g = js_setParamValue (js_audioParam node $ toJSString "gain") $ inDb g

setQ :: JSVal -> Double -> WebAudioContext -> IO ()
setQ node q = js_setParamValue (js_audioParam node $ toJSString "Q") q

instantiateSourceNode :: SourceNodeSpec -> WebAudioContext -> IO Node
instantiateSourceNode Silent ctx = do
  sampleRate <- js_sampleRate ctx
  buffer <- js_createAudioBuffer 1 (ceiling $ sampleRate * 10) sampleRate ctx
  channelData <- js_channelData buffer 0
  js_typedArrayFill channelData 0
  src <- js_createBufferSource ctx
  js_setField src (toJSString "buffer") $ pToJSVal buffer
  js_setField src (toJSString "loop") $ pToJSVal True
  return $ AudioBufferSourceNode src
instantiateSourceNode (Oscillator t f) ctx = do
  osc <- js_createOscillator ctx
  js_setField osc (toJSString "type") $ pToJSVal t
  setFrequencyHz osc f ctx
  return $ OscillatorNode osc

instantiateSourceSinkNode :: SourceSinkNodeSpec -> WebAudioContext -> IO Node
instantiateSourceSinkNode (Filter spec) ctx = do
  filter <- js_createBiquadFilter ctx
  configureBiquadFilterNode spec filter ctx
  return $ BiquadFilterNode filter
instantiateSourceSinkNode (Delay t) ctx = do
  delay <- js_createDelay ctx $ inSec t -- createDelay needs a maxDelayTime
  js_setParamValue (js_audioParam delay (toJSString "delayTime")) (inSec t) ctx
  return $ DelayNode delay
instantiateSourceSinkNode (Compressor thr kne rat red att rel) ctx = do
  comp <- js_createDynamicsCompressor ctx
  let setProp n v = js_setParamValue (js_audioParam comp (toJSString n)) v ctx
  setProp "threshold" $ inDb thr
  setProp "knee" $ inDb kne
  setProp "ratio" $ inDb rat
  js_setField comp (toJSString "reduction") $ pToJSVal $ inDb red
  setProp "attack" $ inSec att
  setProp "release" $ inSec rel
  return $ DynamicsCompressorNode comp
instantiateSourceSinkNode (Gain g) ctx = do
  gain <- js_createGain ctx
  js_setParamValue (js_audioParam gain (toJSString "gain")) (inAmp g) ctx
  return $ GainNode gain
instantiateSourceSinkNode (WaveShaper curve oversample) ctx = do
  shaper <- js_createWaveShaper ctx
  curveArray <- toJSArray $ fmap pToJSVal curve
  typedCurveArray <- js_typedArrayFromArray curveArray
  js_setField shaper (toJSString "curve") $ pToJSVal typedCurveArray
  js_setField shaper (toJSString "oversample") $ pToJSVal oversample
  return $ WaveShaperNode shaper

configureBiquadFilterNode :: FilterSpec -> JSVal -> WebAudioContext -> IO ()
configureBiquadFilterNode (LowPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "lowpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (HighPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "highpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (BandPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "bandpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (LowShelf f g) node ctx =
  js_setField node (toJSString "type") (toJSString "lowshelf") >> setFrequencyHz node f ctx >> setGainDb node g ctx
configureBiquadFilterNode (HighShelf f g) node ctx =
  js_setField node (toJSString "type") (toJSString "highshelf") >> setFrequencyHz node f ctx >> setGainDb node g ctx
configureBiquadFilterNode (Peaking f q g) node ctx =
  js_setField node (toJSString "type") (toJSString "peaking") >> setFrequencyHz node f ctx >> setQ node q ctx >> setGainDb node g ctx
configureBiquadFilterNode (Notch f q) node ctx =
  js_setField node (toJSString "type") (toJSString "notch") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (AllPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "allpass") >> setFrequencyHz node f ctx >> setQ node q ctx

instantiateSinkNode :: SinkNodeSpec -> WebAudioContext -> IO Node
instantiateSinkNode Destination ctx = js_destination ctx >>= return . DestinationNode 

audioParamNode :: Node -> String -> Node
audioParamNode node paramName =
  AudioParamNode $ pToJSVal $ js_audioParam (jsval node) (toJSString paramName)

connect :: Node -> Node -> IO ()
connect from to
  | isSinkNode from = error $ (show from) ++ " can't be connect source."
  | isSourceNode to = error $ (show to) ++ " can't be connect target." 
  | otherwise   = js_connect (jsval from) (jsval to)

disconnect :: Node -> Node -> IO ()
disconnect from to
  | isSinkNode from = error $ (show from) ++ " can't be disconnect source."
  | isSourceNode to = error $ (show to) ++ " can't be disconnect target." 
  | otherwise   = js_disconnect (jsval from) (jsval to)

disconnectAll :: Node -> IO ()
disconnectAll x
  | isSinkNode x = return ()
  | otherwise  = js_disconnectAll (jsval x)

start :: Time -> Node -> IO ()
start t x
  | isSourceNode x = js_start (jsval x) $ inSec t
  | otherwise  = return ()

stop :: Time -> Node -> IO ()
stop t x
  | isSourceNode x = js_stop (jsval x) $ inSec t
  | otherwise  = return ()

onended :: Node -> (Node -> IO ()) -> IO ()
onended n cb = do
  onend <- asyncCallback1 $ \_ -> cb n
  js_onended (jsval n) onend
  -- haskell doesn't need the reference to the callback anymore as js_onended took
  -- it and saved it in the event handler
  releaseCallback onend

setParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
setParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_setParamValueAtTime param value $ inSec time

linearRampToParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
linearRampToParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_linearRampToParamValueAtTime param value $ inSec time

exponentialRampToParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
exponentialRampToParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_exponentialRampToParamValueAtTime param value $ inSec time

setParamValueCurveAtTime :: Node -> String -> [Double] -> Time -> Time -> IO ()
setParamValueCurveAtTime node paramName curve startTime duration = do
  curveArray <- toJSArray $ fmap pToJSVal curve
  typedCurveArray <- js_typedArrayFromArray curveArray
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_setParamValueCurveAtTime param typedCurveArray (inSec startTime) (inSec duration)
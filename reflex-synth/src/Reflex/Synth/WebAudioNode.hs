module Reflex.Synth.WebAudioNode where

import GHCJS.DOM.Types(toJSString)
import GHCJS.Marshal
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal)
import qualified Reflex.Synth.Foreign as F
import Control.Monad (mapM)
import Data.Char (toLower)
import qualified GHCJS.Prim as Prim (toJSString)
import GHCJS.Prim (toJSArray)


import Reflex.Synth.NodeSpec

data WebAudioNode = WebAudioNode NodeSpec JSVal | NullAudioNode

-- Might use this eventually...
--type JSNodeRef = (Either JSVal [JSVal],JSVal) -- fst: node that can be 'started' (if there is one), snd: node that can connect to a subsequent node

createBiquadFilter:: Filter -> IO WebAudioNode
createBiquadFilter (NoFilter) = createGain 0
createBiquadFilter (Filter filtType f q g) = do
  x <- F.createBiquadFilter
  let y = WebAudioNode (FilterNode (Filter filtType f q g)) x
  setFrequency f y
  setFilterQ q y
  setGain g y
  setFilterType filtType y
  return y

createCompressorNode:: Compressor -> IO (WebAudioNode)
createCompressorNode (Compressor a b c d e) = do
  ref <- F.createCompressorNode (pToJSVal a) (pToJSVal b) (pToJSVal c) (pToJSVal d) (pToJSVal e)
  return $ WebAudioNode (CompressorNode $ Compressor a b c d e) ref

createDelayNode::Double -> IO WebAudioNode
createDelayNode delay = do
  delayNode <- F.createDelay
  F.setDelay delayNode delay
  return (WebAudioNode (DelayNode delay) delayNode)

createEnvelope:: Envelope -> IO WebAudioNode
createEnvelope (Custom points dur) = do
  gain <- createGain 0
  gainGain <- F.getGain (getJSVal gain)
  setAmp 0.0 gain
  array<- toJSArray $ fmap pToJSVal points
  F.setValueCurveAtTime gainGain array 0 dur
  return (WebAudioNode (EnvelopeNode (Custom points dur)) (getJSVal gain))


createWaveShaperNode:: WaveShaper -> IO WebAudioNode
createWaveShaperNode (ClipAt db) = F.createClipAtWaveShaper (pToJSVal db) >>= return . WebAudioNode (WaveShaperNode $ ClipAt db)

createOscillator :: Oscillator -> IO WebAudioNode
createOscillator (Oscillator t freq db) = do
  osc <- F.createOscillator (Prim.toJSString $ fmap toLower $ show t) (pToJSVal freq) (pToJSVal db)
  -- F.setOscillatorType (Prim.toJSString $ fmap toLower $ show t) osc  -- Web Audio won't accept 'Sine' must be 'sine'
  -- F.setFrequency freq osc
  -- g <- F.createGain
  -- F.setAmp 0 g
  -- F.connect osc g
  -- F.startNode osc
  return (WebAudioNode (OscillatorNode $ Oscillator t freq db) osc)
createOscillator (Oscillator' t env db) = do
  osc <- F.createOscillator (Prim.toJSString $ fmap toLower $ show t) (pToJSVal (0::Int)) (pToJSVal db)
  envelope <- createEnvelope env
  freq <- F.getFrequency osc
  F.connect freq (getJSVal envelope)
  return (WebAudioNode (OscillatorNode $ Oscillator' t env db) osc)


createGain :: Double -> IO WebAudioNode
createGain g = do
  x <- F.createGain
  F.setGain g x
  return (WebAudioNode (GainNode g) x)

createSilentNode::IO WebAudioNode
createSilentNode = F.createSilentNode >>= return . WebAudioNode (SilentNode)


createBufferNode :: Buffer -> IO WebAudioNode
createBufferNode (File path) = do
  x <- F.createBufferSourceNodeFromURL (Prim.toJSString path)
  return (WebAudioNode (BufferNode $ File path) x)
createBufferNode (LoadedFile inputId (PlaybackParam s e l)) = do
  let s'=  pToJSVal s
  let e'= pToJSVal e
  let l'= pToJSVal l
  x <- F.createBufferSourceNodeFromID (Prim.toJSString inputId) s' e' l'
  return (WebAudioNode (BufferNode $ LoadedFile inputId $ PlaybackParam s e l) x)

createScriptProcessorNode:: DSPEffect -> IO (WebAudioNode)
createScriptProcessorNode (DistortAtDb db) = F.getDistortAtDbFunc (pToJSVal db) >>= F.createScriptProcessorNode >>= return . WebAudioNode (ScriptProcessorNode $ DistortAtDb db)

createAsrEnvelope :: Double -> Double -> Double -> IO WebAudioNode
createAsrEnvelope a s r = do
  now <- F.getCurrentTime
  n <- createGain 0
  setAmp 0.0 n
  linearRampToGainAtTime 0.0 now n
  linearRampToGainAtTime 1.0 (now+a) n
  linearRampToGainAtTime 1.0 (now+a+s) n
  linearRampToGainAtTime 0.0 (now+a+s+r) n
  return n

getDestination :: IO WebAudioNode
getDestination = do
  x <- F.getDestination
  return $ WebAudioNode Destination x

getJSVal::WebAudioNode -> JSVal
getJSVal (WebAudioNode _ x) = x
getJSVal (NullAudioNode) = error "no JSVal for null audio node"

setGain :: Double -> WebAudioNode -> IO ()
setGain g (WebAudioNode (FilterNode _) x) = F.setAmp g x -- filter gain values in Web Audio API are already in dB
setGain g (WebAudioNode (GainNode _) x) = F.setGain g x
setGain _ _ = putStrLn "warning: unmatched pattern in Reflex.Synth.Types.setGain"

setAmp:: Double -> WebAudioNode -> IO ()
-- setAmp a (WebAudioNode (FilterNode _) x) = F.setAmp a x -- this doesn't make sense: filter.gain.value expects db
setAmp a (WebAudioNode (GainNode _) x) = F.setAmp a x
setAmp a (WebAudioNode (OscillatorNode _) x) = F.setOscillatorAmp x a
setAmp _ _ = putStrLn "warning: unmatched pattern in Reflex.Synth.Types.setAmp"

setBufferNodeLoop:: WebAudioNode -> Bool -> IO ()
setBufferNodeLoop (WebAudioNode _ r) b = F.setBufferNodeLoop r (pToJSVal b)


setFrequency:: Double -> WebAudioNode -> IO ()
setFrequency f (WebAudioNode (FilterNode _) x) = F.setFrequency f x
-- @setting freq of other ugens too...

setFilterQ :: Double -> WebAudioNode -> IO ()
setFilterQ q (WebAudioNode (FilterNode _) x) = F.setFilterQ q x
setFilterQ _ _ = error "can't setFilterQ on non-filter"

setFilterType :: FilterType -> WebAudioNode-> IO ()
setFilterType filtType (WebAudioNode (FilterNode _) x) = F.setFilterType (Prim.toJSString $ fmap toLower $ show filtType) x
setFilterType _ _ = error "cannot set filter type of a non-filter"

setAmpAtTime:: Double -> Double -> WebAudioNode -> IO ()
setAmpAtTime val t (WebAudioNode _ node) = F.setAmpAtTime val t node
setAmpAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"

linearRampToGainAtTime:: Double -> Double -> WebAudioNode -> IO ()
linearRampToGainAtTime val t (WebAudioNode _ node) = F.linearRampToGainAtTime val t node
linearRampToGainAtTime _ _ NullAudioNode = error "Cannot set gain of a null node"


startNode :: WebAudioNode -> IO ()
startNode (WebAudioNode (AdditiveNode _) r) = F.startNodes r  -- @this may not be the best..
startNode (WebAudioNode (GainNode _) _) = error "Gain node cannot bet 'started' "
startNode (WebAudioNode (MediaNode s) _) = F.playMediaNode (toJSString s) -- if you call 'start' on a MediaBufferNode a js error is thrown by the WAAPI
startNode (WebAudioNode (OscillatorNode (Oscillator _ _ _)) r) = F.startNode r
startNode (WebAudioNode (BufferNode (LoadedFile a (PlaybackParam b c d))) x) = do
  F.playBufferNode (toJSString a) (pToJSVal b) (pToJSVal c) (pToJSVal d) x
startNode (WebAudioNode _ ref) = F.startNode ref
startNode (WebAudioNode (CompressorNode _) _) = error "Compressor node cannot be started"

stopNodeByID:: String -> IO ()
stopNodeByID s = F.stopNodeByID (toJSString s)

stopOverlappedSound:: String -> IO()
stopOverlappedSound = F.stopOverlappedSound .  toJSString

disconnect:: WebAudioNode -> WebAudioNode -> IO ()
disconnect (WebAudioNode _ a) (WebAudioNode _ b) = F.disconnect a b

disconnectAll::WebAudioNode -> IO ()
disconnectAll (WebAudioNode _ a) = F.disconnectAll a

disconnectAllAtTime:: WebAudioNode -> Double -> IO ()
disconnectAllAtTime (WebAudioNode _ x) t = F.disconnectAllAtTime x (pToJSVal t)

createNode:: NodeSpec -> IO WebAudioNode
createNode (FilterNode x) = createBiquadFilter x
createNode (GainNode d) = createGain d
createNode (Destination) = error "cannot create destination node"
createNode (AdditiveNode xs) = createAdditiveNode xs
createNode (OscillatorNode x) = createOscillator x
createNode (BufferNode x) = createBufferNode x
createNode (MediaNode s) = createMediaNode s
createNode (CompressorNode x) = createCompressorNode x
createNode(WaveShaperNode x) = createWaveShaperNode x
createNode (ConvolverNode x) = createConvolverNode x

createMediaNode:: String -> IO WebAudioNode
createMediaNode s = F.createMediaNode (toJSString s) >>= return . (WebAudioNode (MediaNode s))

createAdditiveNode:: [NodeSpec] -> IO WebAudioNode
createAdditiveNode xs = do
  nodes <- sequence $ fmap createNode xs -- IO [WebAudioNode]
  ref <- toJSArray $ fmap getJSVal nodes
  -- g <- F.createGain
  -- F.setAmp 0 g
  -- sequence (fmap startNode nodes)
  -- mapM (((flip F.connect) g) . getJSVal) nodes
  return (WebAudioNode (AdditiveNode xs) ref) -- returning the gain node's

createConvolverNode :: Buffer -> IO WebAudioNode
createConvolverNode (File s) = F.createConvolverNode (toJSString s) >>= return . WebAudioNode (ConvolverNode $ File s)
createConvolverNode (LoadedFile _ _) = error "does not yet support loaded file for convolver*"

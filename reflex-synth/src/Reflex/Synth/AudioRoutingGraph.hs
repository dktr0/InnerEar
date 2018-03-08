module Reflex.Synth.AudioRoutingGraph (
) where

import Reflex.Synth.Spec
import Reflex.Synth.Graph
import GHCJS.Types
import qualified Data.Map as Map

newtype WebAudioContext = WebAudioContext JSVal

newtype AudioNode = AudioNode JSVal

newtype OscillatorNode = OscillatorNode AudioNode

newtype BiquadFilterNode = BiquadFilterNode AudioNode
newtype DelayNode = DelayNode AudioNode
newtype DynamicsCompressorNode = DynamicsCompressorNode AudioNode

-- AudioContext node creation
foreign import javascript safe 
  "$r = new (window.AudioContext || window.webkitAudioContext);"
  js_newAudioContext :: IO WebAudioContext

-- source nodes
foreign import javascript safe
  "$1.createOscillator()"
  js_newDefaultOscillator :: WebAudioContext -> IO OscillatorNode
foreign import javascript safe
  "new OscillatorNode($4, {type: $1, detune: $2, frequency: $3})"
  js_newOscillator :: (FrequencyInHz f) => OscillatorType -> f -> WebAudioContext -> IO OscillatorNode

-- intermediate nodes
-- Q: 1, detune: 0, frequency: 350, gain: 0
foreign import javascript safe
  "$1.createBiquadFilter()"
  js_newDefaultBiquadFilter :: WebAudioContext -> IO BiquadFilterNode
foreign import javascript safe
  "new BiquadFilterNode($2, $1)"
  js_newBiquadFilter :: FilterSpec -> WebAudioContext -> IO BiquadFilterNode
  
foreign import javascript safe
  "$1.createDelay()"
  js_newDefaultDelay :: WebAudioContext -> IO DelayNode
foreign import javascript safe
  "new DelayNode($3, {delayTime: $1, maxDelayTime: $2})"
  js_newDelay :: (TimeInSeconds t) => t -> t -> WebAudioContext -> IO DelayNode

foreign import javascript safe
  "$1.createDynamicsCompressor()"
  js_newDefaultDynamicsCompressor :: WebAudioContext -> IO DynamicsCompressorNode
foreign import javascript safe
  "new DynamicsCompressorNode($2, $1)"
  js_newDynamicsCompressor :: CompressorSpec -> WebAudioContext -> IO DynamicsCompressorNode

-- TODO these arguments may only be newtypes around jsval or other primitives that convert to jsval, no fancy marshalling

-- sink nodes
foreign import javascript safe
  "$1.destination"
  js_destinationNode :: WebAudioContext -> AudioNode


foreign import javascript safe
  "$1.connect($2)"
  js_connect :: AudioNode -> AudioNode -> IO ()

type AudioNodeConstructor = WebAudioContext -> IO AudioNode

compile :: Synth a -> IO WebAudioContext
compile _ = undefined

newSourceNodeFromSpec :: SourceNodeSpec -> WebAudioContext -> IO AudioNode
newSourceNodeFromSpec Silent = js_newSilentNode
newSourceNodeFromSpec (Oscillator oscType freq) = js_newOscillator (pToJSVal oscType) (inHz freq)

newSinkNodeFromSpec :: SinkNodeSpec -> WebAudioContext -> IO AudioNode
newSinkNodeFromSpec Destination = js_destination

compileNode :: Graph -> IO (Either Reference AudioNodeConstructor)
compileNode (Source (SourceRef ref)) = return $ Left ref
compileNode (Source (SourceSpec spec)) = return $ Right $ newSourceNodeFromSpec spec
compileNode (Sink (SinkRef ref) _) = return $ Left ref
compileNode (Sink (SinkParamRef g param) _) = return $ Right $ 

type CompiledEnv = Map.Map Reference (Either Reference AudioNodeConstructor)

compileEnv :: Env -> IO CompiledEnv
compileEnv env = Map.map compileNode env

compileGraph :: Env -> Graph -> WebAudioContext -> IO ()
compileGraph e (Source (SourceRef ref)) = Map.lookup ref e

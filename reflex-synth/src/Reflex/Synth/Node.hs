module Reflex.Synth.Node where

import Control.Monad

import Reflex.Synth.Foreign
import Reflex.Synth.Node

-- | A NodeDef is a representation of how to instantiate a some node in
-- the Web Audio API, the numerical position in a list of nodes that
-- should be connected to this node as sources, and how the node should be
-- started (i.e. do nothing, call start(), set up envelopes, etc)

data NodeDef = NodeDef {
  instantiate :: IO JSVal,
  start :: JSVal -> IO (),
  sources :: [Int]
}

gainNodeDef :: Double -> NodeDef
gainNodeDef g = NodeDef {
  instantiate = createGain >>= setGain g,
  start = const $ return (),
  sources = []
}

peakingFilterNodeDef :: Double -> Double -> Double -> NodeDef
peakingFilterNodeDef f q g = NodeDef {
  instantiate = createBiquadFilter >>= setFrequency f >>= setFilterQ q >>= setGain g >>= setFilterType "Peaking",
  start = const $ return (),
  sources = []
}

sineOscillatorNodeDef :: Double -> NodeDef
sineOscillatorNodeDef f = NodeDef {
  instantiate = createOscillator >>= setFrequency f,
  start = startNode >=> return (),
  sources = []
}

bufferNodeDef :: Buffer -> NodeDef
bufferNodeDef b = NodeDef {
  instantiate = createBufferSourceNodeFromURL (Prim.toJSString b),
  start = startNode >=> return (),
  sources = []
}

destinationDef :: NodeDef
destinationDef = NodeDef {
  instantiate = getDestination,
  start = const $ return (),
  sources = []
}

-- | And a Node is a NodeDef that has been instantiated in the browser (using createNodeFromDef).

data Node = Node {
  nodeDef :: NodeDef,
  nodeVal :: JSVal
}

createNodeFromDef :: NodeDef -> IO Node
createNodeFromDef x = Node x <$> instantiate x

startNode :: Node -> IO ()
startNode x = start (nodeDef x) $ nodeVal x

connectNodes :: Node -> Node -> IO ()
connectNodes x y = connect (nodeVal x) (nodeVal y)

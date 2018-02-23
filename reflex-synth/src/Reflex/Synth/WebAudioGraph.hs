module Reflex.Synth.WebAudioGraph where

import Reflex.Synth.NodeSpec
import Reflex.Synth.WebAudioNode
import qualified Reflex.Synth.Foreign as F


-- | For representing WebAudio Graphs - to be understood as hooking up a sequence of nodes/ugens
-- for instance, the web audio graph:
--   oscillator.connect(gain)
--   gain.connect(compressor)
-- would be represented (roughly) as:
-- WebAudioGraph' oscillator (WebAudioGraph' gain (WebAudioGraph compressor))

data WebAudioGraph = WebAudioGraph WebAudioNode |
 WebAudioGraph' WebAudioNode WebAudioGraph |
 WebAudioGraph'' WebAudioGraph WebAudioGraph |
 WebAudioGraph'''  [WebAudioGraph] WebAudioNode -- list of graphs played in parallel, and the last node should be a Gain node to mix them all

-- Get the first node in a Graph - usually the first node in a graph function is the 'source' (such as a buffer or oscillator)
-- which needs to be 'started' in the Web Audio API to hear anything.
getFirstNode:: WebAudioGraph -> WebAudioNode
getFirstNode (WebAudioGraph n) = n
getFirstNode (WebAudioGraph' n _) = n
getFirstNode (WebAudioGraph'' n _) = getFirstNode n
getFirstNode (WebAudioGraph''' _ _) = error "getFirstNode:  Cannot get the first node of a mixed signal ( a WebAudioGraph''' )"

-- Gets the lst node in a WebAudioGraph - allows for tacking more ugens/nodes onto the end of a graph
getLastNode:: WebAudioGraph -> WebAudioNode
getLastNode (WebAudioGraph n) = n
getLastNode (WebAudioGraph' _ n) = getLastNode n
getLastNode (WebAudioGraph'' _ n) = getLastNode n
getLastNode (WebAudioGraph''' xs x) = x

connectGraphToDest:: WebAudioGraph -> IO ()
connectGraphToDest g = do
  let l = getLastNode g
  dest <- getDestination
  connect l dest
  return ()

startFirstNode :: WebAudioGraph -> IO()
startFirstNode g = let f = getFirstNode g in startNode f

connect :: WebAudioNode -> WebAudioNode -> IO WebAudioGraph
connect (WebAudioNode Destination _) _ = error "destination can't be source of connection"
connect NullAudioNode _ = return (WebAudioGraph NullAudioNode)
connect a NullAudioNode = return (WebAudioGraph a)
connect (WebAudioNode (AdditiveNode xs) r) (WebAudioNode a y) = do
  F.connectAdditiveNode r y
  return $ WebAudioGraph' (WebAudioNode (AdditiveNode xs) r) $ WebAudioGraph (WebAudioNode a y)
connect (WebAudioNode x y) (WebAudioNode (ScriptProcessorNode e) y') = do
  F.spConnect y y'
  return $ WebAudioGraph' (WebAudioNode x y) $ WebAudioGraph (WebAudioNode (ScriptProcessorNode e) y')
connect (WebAudioNode xt x) (WebAudioNode yt y) = do
  F.connect x y
  return $ WebAudioGraph' (WebAudioNode xt x) (WebAudioGraph (WebAudioNode yt y))

startGraph :: WebAudioGraph -> IO ()
startGraph (WebAudioGraph''' xs n)= do
  dest <- getDestination
  connect n dest
  sequence $ fmap (startNode . getFirstNode) xs
  return ()
startGraph (WebAudioGraph'' (WebAudioGraph''' xs b) c) = do
  dest <- getDestination
  let l = getLastNode c
  connect l dest
  sequence $ fmap (startNode . getFirstNode) xs
  return ()
startGraph (WebAudioGraph' a (WebAudioGraph''' _ _)) = error "Error connecting one node to multiple. (see startGraph, in Types.hs)"
startGraph a = do
  let f = getFirstNode a
  let l = getLastNode a
  dest <- getDestination
  connect l dest
  startNode f

connectGraph :: WebAudioGraph -> IO WebAudioGraph
connectGraph (WebAudioGraph n) = return $ WebAudioGraph n
connectGraph (WebAudioGraph' n (WebAudioGraph n2)) = do
  connect n n2
  return $ (WebAudioGraph' n (WebAudioGraph n2))
connectGraph (WebAudioGraph' n (WebAudioGraph' n2 xs)) = do
  connect n n2
  connectGraph (WebAudioGraph' n2 xs)
  return (WebAudioGraph' n (WebAudioGraph' n2 xs))
connectGraph (WebAudioGraph'' a b) = do
  let aLast = getLastNode a
  let bFirst = getFirstNode b
  connect aLast bFirst
  return (WebAudioGraph'' a b)
connectGraph (WebAudioGraph''' xs n) = do
  sequence $ fmap (\x-> do
    connectGraph x
    connect (getLastNode x) n
    ) xs
  return (WebAudioGraph''' xs n)


disconnectGraphAtTimeMaybe:: WebAudioGraph -> Maybe Double -> IO ()
disconnectGraphAtTimeMaybe a (Just b) = disconnectGraphAtTime a b
disconnectGraphAtTimeMaybe _ Nothing = return ()

disconnectGraphAtTime :: WebAudioGraph -> Double -> IO ()
disconnectGraphAtTime (WebAudioGraph w) t = disconnectAllAtTime w t
disconnectGraphAtTime (WebAudioGraph' n g) t = do
  disconnectAllAtTime n t
  disconnectGraphAtTime g t
disconnectGraphAtTime (WebAudioGraph'' g1 g2) t = do
  disconnectGraphAtTime g1 t
  disconnectGraphAtTime g2 t
disconnectGraphAtTime (WebAudioGraph''' xs g) t = do
  mapM ((flip disconnectGraphAtTime) t) xs
  disconnectAllAtTime g t

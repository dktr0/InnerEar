





data Synthstance = Synthstance

type Synstance = Synthstance

nodePropToNodes :: NodeProps -> IO Node
nodePropToNodes (SourceSpec x) = sourceNodeSpecToNode x
nodePropToNodes (SourceSinkSpec x) = nodeSpec x
nodePropToNodes (SinkSpec x) = sinkNodeSpecToNode x

sourceNodeSpecToNode :: SourceNodeSpec -> IO Node
sourceNodeSpecToNode Silent = getSilentNode
sourceNodeSpecToNode (Oscillator t f) = getOscillatorNode t f

nodeSpecToNode :: NodeSpec -> IO Node
nodeSpecToNode (Gain x) = getGainNode x
-- etc

sinkNodeSpecToNode :: NodeSpec -> IO Node
sinkNodeSpecToNode Destination = getDestination

connectGraph :: Map Int Node -> Graph -> IO ()
connectGraph m (Source _) = return ()
connectGraph m (Sink (RefToNode i) g) = do
  connect (getNodeId g)  (m!!i)
  connectGraph m g
connectGraph m (Sink (RefToParamOfNode i p) g) = do
  connectToParamOfNode (m!!(getNodeId g)) p (m!!i)
  connectGraph m g
connectGraph m (SourceSink (RefToNode i) g) = do
  connect (getNodeId g)  (m!!i)
  connectGraph m g
connectGraph m (SourceSink (RefToParamOfNode i p) g) = do
  connectToParamOfNode (m!!(getNodeId g)) p (m!!i)
  connectGraph m g

instantiateChange :: Map Int Node -> Change -> IO ()
instantiateChange m (SetValue g p v t) = setValue (m!!(getNodeId g)) p v t
instantiateChange m (LinearRampToValue g p v t) = linearRampToValue (m!!(getNodeId g)) p v t
instantiateChange m (ExponentialRampToValue g p v t) = exponentialRampToValue (m!!(getNodeId g)) p v t
instantiateChange m (CurveToValue g p v t d) = curveToValue (m!!(getNodeId g)) p v t d

deleteNode :: Maybe Time -> Node -> IO ()
deleteNode (Nothing) _ = return ()
deleteNode (Just t) n = stopDeferred t n >> disconnectAllDeferred t n

instantiateSynth :: Synth a -> IO Synthstance
instantiateSynth x = do
  ns <- mapM nodePropToNodes $ env x
  mapM (connectGraph ns) $ snd (graphs x)
  mapM (instantiateChange ns) $ changes x
  mapM deleteNode $ elems ns
  return Synthstance

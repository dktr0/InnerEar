module Today where

data Node =
  Destination JSVal |
  AudioBufferSourceNode JSVal |
  GainNode JSVal

nodeJSVal :: Node -> JSVal
nodeJSVal (Destination x) = x
nodeJSVal (AudioBufferSourceNode x) = x
nodeJSVal (GainNode x) = x

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

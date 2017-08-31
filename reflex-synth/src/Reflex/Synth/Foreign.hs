module Reflex.Synth.Foreign where

import GHCJS.Types (JSVal,JSString)

foreign import javascript safe "___ac = new AudioContext()" createAudioContext:: IO ()
foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "$1.connect($2)" connect :: JSVal -> JSVal -> IO ()
foreign import javascript safe "$1.disconnect($2)" disconnect ::JSVal -> JSVal -> IO()
foreign import javascript safe "$1.disconnect()" disconnectAll::JSVal -> IO()

foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
foreign import javascript safe "$r=___ac.createOscillator()" createOscillator :: IO JSVal

foreign import javascript safe "loadUserSoundFile()" loadUserSoundFile :: IO ()

foreign import javascript safe "$r= createMediaNode($1)" createMediaNode :: JSString -> IO JSVal


foreign import javascript safe "$2.gain.value = $1; $r=$2" setGain :: Double -> JSVal -> IO JSVal
foreign import javascript safe "$2.frequency.value = $1; $r=$2" setFrequency :: Double -> JSVal -> IO JSVal

foreign import javascript safe "$2.Q.value = $1; $r=$2" setFilterQ :: Double -> JSVal -> IO JSVal
foreign import javascript safe "$2.type = $1; $r=$2" setFilterType :: JSVal -> JSVal -> IO JSVal
foreign import javascript safe "$2.type = $1; $r=$2" setOscillatorType :: JSVal -> JSVal -> IO JSVal

foreign import javascript safe "loadBuffer($1)" loadBuffer:: JSString -> IO ()

foreign import javascript safe "$r = createBufferSourceNodeFromURL($1)" createBufferSourceNodeFromURL :: JSVal -> IO JSVal  -- Js string to IO JSVal...
foreign import javascript safe "$r = createBufferSourceNodeFromID($1)" createBufferSourceNodeFromID :: JSVal -> IO JSVal  -- Js string to IO JSVal...

foreign import javascript safe "setAudioSrc($1)" setAudioSrc :: JSString -> IO ()

foreign import javascript safe "$r = ___ac.currentTime" getCurrentTime :: IO Double

foreign import javascript safe "$3.gain.setValueAtTime($1,$2)" setGainAtTime :: Double -> Double -> JSVal-> IO ()

-- using https://github.com/zacharydenton/noise.js/blob/master/noise.js
foreign import javascript safe "$r=___ac.createWhiteNoise()" createWhiteNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createPinkNoise()" createPinkNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createBrownNoise()" createBrownianNoise :: IO JSVal


foreign import javascript safe "console.log($1);$1.start();$r=$1" startNode :: JSVal -> IO JSVal
foreign import javascript safe "playMediaNode($1)" playMediaNode:: JSString -> IO()


-- takes 'canvas' html elements - us 'toJSVal on the html element'
foreign import javascript safe "drawBufferWaveform($1, $2)" renderAudioWaveform :: JSVal -> JSVal -> IO()


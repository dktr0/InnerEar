module Reflex.Synth.Foreign where 

-- InnerEar.WebAudio.Foreign where

import GHCJS.Types (JSVal)

foreign import javascript safe "___ac = new AudioContext()" createAudioContext:: IO ()
foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "$1.connect($2)" connect :: JSVal -> JSVal -> IO ()
foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
foreign import javascript safe "$2.gain.value = $1" setGain :: Double -> JSVal -> IO ()

foreign import javascript safe "$1.frequency.value = $2" setF :: JSVal -> Double -> IO()
foreign import javascript safe "$1.Q.value = $2" setQ :: JSVal -> Double -> IO()
foreign import javascript safe "$1.gain.value = $2" setFilterGain :: JSVal -> Double -> IO()

foreign import javascript safe "$r = ___ac.currentTime" getCurrentTime :: IO Double

foreign import javascript safe "$3.gain.setValueAtTime($1,$2)" setGainAtTime :: Double -> Double -> JSVal-> IO ()

-- using https://github.com/zacharydenton/noise.js/blob/master/noise.js
foreign import javascript safe "$r=___ac.createWhiteNoise()" createWhiteNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createPinkNoise()" createPinkNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createBrownNoise()" createBrownianNoise :: IO JSVal

foreign import javascript safe "$1.start()" startNode :: JSVal -> IO ()
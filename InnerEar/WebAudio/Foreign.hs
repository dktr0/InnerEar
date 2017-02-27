module InnerEar.WebAudio.Foreign where

import GHCJS.Types (JSVal)

foreign import javascript safe "___ac = new AudioContext()" :: IO ()
foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "$1.connect($2)" connect :: JSVal -> JSVal -> IO ()
foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
foreign import javascript safe "$2.gain.value = $1" setGain :: Double -> JSVal -> IO ()

-- using https://github.com/zacharydenton/noise.js/blob/master/noise.js
foreign import javascript safe "$r=___ac.createWhiteNoise()" createWhiteNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createPinkNoise()" createPinkNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createBrownNoise()" createBrownNoise :: IO JSVal

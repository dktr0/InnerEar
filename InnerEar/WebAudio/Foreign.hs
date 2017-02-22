module InnerEar.WebAudio.Foreign where

import GHCJS.Types (JSVal)

foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "$1.connect($2)" connect :: JSVal -> JSVal -> IO ()
foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
foreign import javascript safe "$2.gain.value = $1" setGain :: Double -> JSVal -> IO ()

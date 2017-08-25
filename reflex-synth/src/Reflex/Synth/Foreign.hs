module Reflex.Synth.Foreign where 

-- InnerEar.WebAudio.Foreign where

import GHCJS.Types (JSVal,JSString)
--import qualified GHCJS.Prim as Prim (toJSString)


foreign import javascript safe "___ac = new AudioContext()" createAudioContext:: IO ()
foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "$1.connect($2)" connect :: JSVal -> JSVal -> IO ()

foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
foreign import javascript safe "$r=___ac.createOscillator()" createOscillator :: IO JSVal

foreign import javascript safe "loadUserSoundFile()" loadUserSoundFile :: IO ()


--foreign import javascript safe "$r=___ac.createMediaElementSource(document.getElementById(\"userAudio\"))" createMediaNode :: IO JSVal
--foreign import javascript safe "console.log($1);$r = setTimeout(function(){return ___ac.createMediaElementSource(document.getElementById($1))},2000)" createMediaNode:: JSString -> IO JSVal

foreign import javascript safe "console.log($1);$r = ___ac.createMediaElementSource(document.getElementById($1))" createMediaNode:: JSString -> IO JSVal
--foreign import javascript safe "$r = test($1)" createMediaNode:: JSString -> IO JSVal

--foreign import javascript safe "$r = ___ac.createMediaElementSource($1)" createMediaNode :: JSVal -> IO JSVal




foreign import javascript safe "$2.gain.value = $1" setGain :: Double -> JSVal -> IO ()
foreign import javascript safe "$2.frequency.value = $1" setFrequency :: Double -> JSVal -> IO()

foreign import javascript safe "$2.Q.value = $1" setFilterQ :: Double -> JSVal -> IO()
foreign import javascript safe "$2.type = $1" setFilterType :: JSVal -> JSVal -> IO()
foreign import javascript safe "$2.type = $1" setOscillatorType :: JSVal -> JSVal -> IO()


foreign import javascript safe "$r = getBufferSourceNode($1)" createAudioBufferSourceNode :: JSVal -> IO JSVal  -- Js string to IO JSVal...

foreign import javascript safe "$r = ___ac.currentTime" getCurrentTime :: IO Double

foreign import javascript safe "$3.gain.setValueAtTime($1,$2)" setGainAtTime :: Double -> Double -> JSVal-> IO ()

-- using https://github.com/zacharydenton/noise.js/blob/master/noise.js
foreign import javascript safe "$r=___ac.createWhiteNoise()" createWhiteNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createPinkNoise()" createPinkNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createBrownNoise()" createBrownianNoise :: IO JSVal


foreign import javascript safe "console.log($1);$1.start()" startNode :: JSVal -> IO ()
foreign import javascript safe "document.getElementById(\"userAudio\").play()" playMediaNode:: IO()


-- takes 'canvas' html elements - us 'toJSVal on the html element'
foreign import javascript safe "drawBufferWaveform($1, $2)" renderAudioWaveform :: JSVal -> JSVal -> IO()


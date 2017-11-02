module Reflex.Synth.Foreign where

-- InnerEar.WebAudio.Foreign where

import GHCJS.Types (JSVal,JSString)
--import qualified GHCJS.Prim as Prim (toJSString)


foreign import javascript safe "___ac = new AudioContext()" createAudioContext:: IO ()
foreign import javascript safe "startSilentNode()" startSilentNode:: IO ()
foreign import javascript safe "$r=___ac.destination" getDestination :: IO JSVal
foreign import javascript safe "console.log('connecting here...');$1.connect($2)" connect :: JSVal -> JSVal -> IO ()
spConnect = connect -- temporary... (?maybe not?)
foreign import javascript safe "connectAdditiveNode($1,$2)" connectAdditiveNode:: JSVal -> JSVal -> IO ()
foreign import javascript safe "$1.disconnect($2)" disconnect ::JSVal -> JSVal -> IO()
foreign import javascript safe "$1.disconnect()" disconnectAll::JSVal -> IO()

foreign import javascript safe "setTimeout(function () {$1.disconnect()}, $2*1000)" disconnectAllAtTime:: JSVal -> JSVal -> IO ()

foreign import javascript safe "$r=___ac.createGain()" createGain :: IO JSVal
foreign import javascript safe "$r=___ac.createBiquadFilter()" createBiquadFilter :: IO JSVal
-- foreign import javascript safe "$r=___ac.createOscillator()" createOscillator :: IO JSVal
foreign import javascript safe "$r = new Oscillator($1, $2, $3)" createOscillator :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript safe " $r=createCompressorNode($1, $2, $3, $4, $5)" createCompressorNode:: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal


foreign import javascript safe "overlappedDictionary[$1]=$2" adddToOverlappedDictionary:: JSString -> JSVal -> IO ()
foreign import javascript safe "loadUserSoundFile()" loadUserSoundFile :: IO ()

foreign import javascript safe "$r= createMediaNode($1)" createMediaNode :: JSString -> IO JSVal


foreign import javascript safe "setGain($1, $2)" setGain :: Double -> JSVal -> IO ()   -- Setting Gain to a DB value (NOT AMPLITUDE)
foreign import javascript safe "$2.gain.value = $1" setAmp::Double -> JSVal -> IO ()
foreign import javascript safe "$1.loop=$2" setBufferNodeLoop :: JSVal -> JSVal -> IO ()
foreign import javascript safe "$1.setAmp($2)" setOscillatorAmp:: JSVal -> Double -> IO ()
foreign import javascript safe "$2.frequency.value = $1" setFrequency :: Double -> JSVal -> IO()

foreign import javascript safe "$2.Q.value = $1" setFilterQ :: Double -> JSVal -> IO()
foreign import javascript safe "$2.type = $1" setFilterType :: JSVal -> JSVal -> IO()
foreign import javascript safe "$2.type = $1" setOscillatorType :: JSVal -> JSVal -> IO()

foreign import javascript safe "loadBuffer($1)" loadBuffer:: JSString -> IO ()

foreign import javascript safe "$r = createBufferSourceNodeFromURL($1)" createBufferSourceNodeFromURL :: JSVal -> IO JSVal  -- Js string to IO JSVal...
foreign import javascript safe "$r =  createBufferSourceNodeFromID($1,$2,$3,$4)" createBufferSourceNodeFromID :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal  -- Js string to IO JSVal...
foreign import javascript safe "$r = createScriptProcessorNode($1)" createScriptProcessorNode :: JSVal -> IO (JSVal)
foreign import javascript safe "$r = createClipAtWaveShaper($1)" createClipAtWaveShaper :: JSVal -> IO JSVal
foreign import javascript safe "$r = createConvolverNode($1)" createConvolverNode:: JSString -> IO JSVal

foreign import javascript safe "setAudioSrc($1)" setAudioSrc :: JSString -> IO ()

foreign import javascript safe "$r = ___ac.currentTime" getCurrentTime :: IO Double

foreign import javascript safe "$3.gain.setValueAtTime($1,$2)" setAmpAtTime :: Double -> Double -> JSVal-> IO ()
foreign import javascript safe "$3.gain.linearRampToValueAtTime($1, $2)" linearRampToGainAtTime:: Double -> Double -> JSVal -> IO ()

foreign import javascript safe "$r = ___ac.createOscillator()" createSilentNode:: IO JSVal

foreign import javascript safe "$r = getDistortAtDbFunc($1)" getDistortAtDbFunc:: JSVal -> IO JSVal

foreign import javascript safe "playBufferNode($1, $2, $3, $4, $5)" playBufferNode:: JSString -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

-- using https://github.com/zacharydenton/noise.js/blob/master/noise.js
foreign import javascript safe "$r=___ac.createWhiteNoise()" createWhiteNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createPinkNoise()" createPinkNoise :: IO JSVal
foreign import javascript safe "$r=___ac.createBrownNoise()" createBrownianNoise :: IO JSVal


foreign import javascript safe "startNode($1)" startNode :: JSVal -> IO ()
foreign import javascript safe "startNodes($1)" startNodes:: JSVal -> IO ()  -- JSarray of nodes (in an additive node) loop through and run 'start'
foreign import javascript safe "stopNodeByID($1)" stopNodeByID::JSString -> IO ()
foreign import javascript safe "stopOverlappedSound($1)" stopOverlappedSound:: JSString -> IO ()
foreign import javascript safe "playMediaNode($1)" playMediaNode:: JSString -> IO()


foreign import javascript safe "loadAndDrawBuffer($1,$2)" loadAndDrawBuffer :: JSString -> JSVal -> IO ()

-- takes 'canvas' html elements - us 'toJSVal on the html element'
foreign import javascript safe "renderAudioWaveform($1, $2,0)" renderAudioWaveform :: JSString -> JSVal -> IO()
foreign import javascript safe "drawSineWave($1)" drawSineWave :: JSVal -> IO ()

module Reflex.Synth.Components where

import Reflex.Synth.Graph
import Reflex.Synth.Spec
import GHCJS.Prim(toJSString)

import Reflex.Synth.AudioRoutingGraph

-- Nodes

silent :: SynthBuilder Graph
silent = synthSource Silent

oscillator :: OscillatorType -> Frequency -> SynthBuilder Graph
oscillator oscType freq = synthSource $ Oscillator oscType freq

audioBufferSource :: AudioBuffer -> BufferParams -> SynthBuilder Graph
audioBufferSource b p = synthSource $ AudioBufferSource b p


biquadFilfer :: FilterSpec -> SynthBuilder Graph
biquadFilfer = synthSourceSink . Filter

gain :: Amplitude -> SynthBuilder Graph
gain amp = synthSourceSink $ Gain amp


destination :: SynthBuilder Graph
destination = synthSink Destination


-- Pseudonodes

clipAt :: Amplitude -> SynthBuilder Graph
clipAt amp =
  let nSamples = 65536 :: Int in
  let clip = inAmp amp in
  let portion = (1.0 - clip) / 2.0 in
  let nConstSamples = floor $ portion * fromIntegral nSamples in
  let left = Const nConstSamples (-clip) EmptyArray in
  let mid = listToArraySpec [(2.0 * fromIntegral i) / (fromIntegral $ nSamples - 1) | i <- [nConstSamples..(nSamples-nConstSamples)-1]] in
  let right = Const nConstSamples clip EmptyArray in
  synthSourceSink $ WaveShaper (Right $ mconcat [left, mid, right]) None


-- Envelopes

ampEnv :: Time -> Time -> Amplitude -> Time -> Time -> SynthBuilder Graph
ampEnv a d s st r = do
  g <- gain (Amp 0.0) -- make the node to modulate

  -- linear attack to amp 1 ending at time a
  linearRampToParamValue "gain" 1.0 a g
  -- exp decay to amp s after time a and ending at time a + d
  exponentialRampToParamValue "gain" (inAmp s) (a + d) g
  setParamValue "gain" (inAmp s) (a + d + st) g
  linearRampToParamValue "gain" 0.0 (a + d + st + r) g

asr :: Time -> Time -> Time -> Amplitude -> SynthBuilder Graph
asr a s r amp = do
  g <- gain (Amp 0.0)
  linearRampToParamValue "gain" (inAmp amp) a g
  setParamValue "gain" (inAmp amp) (a+s) g
  linearRampToParamValue "gain" 0 (a+s+r) g

rectEnv :: Time -> Time -> Amplitude -> SynthBuilder Graph
rectEnv ar s amp = asr ar s ar amp


-- Utilities

maybeSynth :: (a -> SynthBuilder b) -> Maybe a -> SynthBuilder ()
maybeSynth f = maybe (return ()) (\x -> f x >> return ())


-- Examples

test :: Synth ()
test = buildSynth $ oscillator Sine (Hz 440) >> gain (Amp 0.5) >> destination >> return ()

test2 :: Synth ()
test2 = buildSynth $ do
  oscillator Sine (Hz 440)
  gain (Amp 0.5)
  destination
  return ()

test3 :: Synth ()
test3 = buildSynth $ do
  osc <- oscillator Sine (Hz 440)
  destination
  return ()

test4 :: Synth ()
test4 = buildSynth $ do
  g <- gain (Amp 0.5)
  setParamValue "gain" 0.0 (Sec 1.0) g
  return ()

test5 :: Synth ()
test5 = buildSynth $ do
  oscillator Sine (Hz 440)
  g <- gain (Db $ -20)
  do -- lfo gain modulation
    oscillator Sine (Hz 2) -- osc.connect(g.gain)
    audioParamSink "gain" g
  destination
  return ()

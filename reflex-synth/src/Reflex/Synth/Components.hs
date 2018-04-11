module Reflex.Synth.Components where

import Reflex.Synth.Graph
import Reflex.Synth.Spec

import qualified Reflex.Synth.Sound as L
import qualified Reflex.Synth.NodeSpec as L

oscillator :: OscillatorType -> Frequency -> SynthBuilder Graph
oscillator oscType freq = synthSource $ Oscillator oscType freq

gain :: Amplitude -> SynthBuilder Graph
gain amp = synthSourceSink $ Gain amp

destination :: SynthBuilder Graph
destination = synthSink Destination

ampEnvelope :: Time -> Time -> Amplitude -> Time -> Time -> SynthBuilder Graph
ampEnvelope a d s st r = do
  g <- gain (Amp 0.0) -- make the node to modulate
  
  -- linear attack to amp 1 ending at time a
  linearRampToParamValue "gain" 1.0 a g
  -- exp decay to amp s after time a and ending at time a + d
  exponentialRampToParamValue "gain" (inAmp s) (a + d) g
  setParamValue "gain" (inAmp s) (a + d + st) g
  linearRampToParamValue "gain" 0.0 (a + d + st + r) g

clipAt :: Amplitude -> SynthBuilder Graph
clipAt amp =
  let nSamples = 65536
  let clip = inAmp amp
  let portion = (1.0 - clip) / 2.0
  let left = Const (floor $ portion * nSamples) (-clip)
  let mid = listToBuffer [(2*i) / (nSamples - 1) | i <- [left..right-1]]
  let right = Const (portion - (floor $ portion * nSamples)) clip
  synthSourceSink $ WaveShaper (Right $ left mid right EmptyBuffer) None

legacySound :: L.Sound -> SynthBuilder ()
legacySound = synthLegacy 

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
  setParamValue g "gain" 0.0 $ Sec 1.0
  return ()
  
test5 :: Synth ()
test5 = buildSynth $ do
  oscillator Sine (Hz 440)
  g <- gain (Db $ -20)
  do -- lfo gain modulation
    oscillator Sine (Hz 2) -- osc.connect(g.gain)
    audioParamSink g "gain"
  destination
  return ()

test6 :: Synth ()
test6 = buildSynth $ legacySound $ L.GainSound (L.OverlappedSound "why?" [n1, n2]) (-20)
  where
    n1 = L.Sound $ L.NodeSource (L.OscillatorNode $ L.Oscillator L.Triangle 440 0.0) (Just 0.8)
    n2 = L.DelayedSound (L.Sound $ L.NodeSource (L.OscillatorNode $ L.Oscillator L.Triangle 880 0.0) (Just 1.0)) 1.0

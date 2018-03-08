module Reflex.Synth.Components where

import Reflex.Synth.Graph
import Reflex.Synth.NodeSpec

oscillator :: OscillatorType -> Frequency -> SynthBuilder ()
oscillator oscType freq = synthSource $ Oscillator oscType freq

gain :: Amplitude -> SynthBuilder ()
gain amp = synthNode $ Gain amp

destination :: SynthBuilder ()
destination = synthSink Destination

ampEnvelope :: Time -> Time -> Amplitude -> Time -> Time -> SynthBuilder ()
ampEnvelope a d s st r = do
  g <- ref $ gain (Amp 0.0) -- make the node to modulate
  
  -- linear attack to amp 1 ending at time a
  linearRampToParamValue g "gain" 1.0 a 
  -- exp decay to amp s after time a and ending at time a + d
  exponentialRampToParamValue g "gain" (getInAmp s) $ a + d
  setParamValue g "gain" (getInAmp s) $ a + d + st
  linearRampToParamValue g "gain" 0.0 $ a + d + st + r
  
test :: Synth ()
test = buildSynth $ oscillator >> gain >> destination

test2 :: Synth ()
test2 = buildSynth $ do
  oscillator
  gain
  destination

test3 :: Synth ()
test3 = buildSynth $ do
  oscillator
  parallelChannels 2 $ \c -> do
    if c == 0
      then gain
      else return ()
  destination

test4 :: Synth ()
test4 = buildSynth $ do
  osc <- ref oscillator
  deref osc
  destination

test5 :: Synth ()
test5 = buildSynth $ do
  lfo <- ref oscillator
  g <- ref gain
  oscillator
  diverge $ do
    gain
    destination
  diverge $ do
    deref lfo
    deref g
    destination
  gain
  destination

test6 :: Synth ()
test6 = buildSynth $ do
  gGain <- ref gain
  oscillator
  g <- branch $ do
    deref gGain
    gain
  h <- branch $ gain
  merge [g, h]
  gain
  destination

test7 :: Synth ()
test7 = buildSynth $ do
  oscillator
  mix $ replicate 3 gain
  return ()

test8 :: Synth ()
test8 = buildSynth $ do
  g <- ref gain
  setParamValue g "gain" 0.0 $ Sec 1.0
  
test9 :: Synth ()
test9 = buildSynth $ do
  oscillator
  diverge $ do
    gain
    destination
  gain
  
test10 :: Synth ()
test10 = buildSynth $ do
  oscillator
  g <- ref gain (Db $ -20)
  deref g -- TODO make a ref that also includes the node in addition to the ref
  diverge $ do -- lfo gain modulation
    oscillator (Hz 2)
    audioParamSink g "gain"
  destination
module Reflex.Synth.Components where

import Reflex.Synth.Graph
import Reflex.Synth.Spec

oscillator :: OscillatorType -> Frequency -> SynthBuilder ()
oscillator oscType freq = synthSource $ Oscillator oscType freq

gain :: Amplitude -> SynthBuilder ()
gain amp = synthNode $ Gain amp

destination :: SynthBuilder ()
destination = synthSink Destination

ampEnvelope :: Time -> Time -> Amplitude -> Time -> Time -> SynthBuilder ()
ampEnvelope a d s st r = do
  g <- ref $ gain (Amp 0.0) -- make the node to modulate
  deref g
  
  -- linear attack to amp 1 ending at time a
  linearRampToParamValue g "gain" 1.0 a 
  -- exp decay to amp s after time a and ending at time a + d
  exponentialRampToParamValue g "gain" (getInAmp s) $ a + d
  setParamValue g "gain" (getInAmp s) $ a + d + st
  linearRampToParamValue g "gain" 0.0 $ a + d + st + r
  
test :: Synth ()
test = buildSynth $ oscillator Sine (Hz 440) >> gain (Amp 0.5) >> destination

test2 :: Synth ()
test2 = buildSynth $ do
  oscillator Sine (Hz 440)
  gain (Amp 0.5)
  destination

test3 :: Synth ()
test3 = buildSynth $ do
  oscillator Sine (Hz 440)
  parallelChannels 2 $ \c -> do
    if c == 0
      then gain (Amp 0.6)
      else return ()
  destination

test4 :: Synth ()
test4 = buildSynth $ do
  osc <- ref $ oscillator Sine (Hz 440)
  deref osc
  destination

test5 :: Synth ()
test5 = buildSynth $ do
  lfo <- ref $ oscillator Sine (Hz 1)
  g <- ref $ gain (Amp 0.5)
  oscillator Sine (Hz 440)
  diverge $ do
    gain (Amp 0.5)
    destination
  diverge $ do
    deref lfo
    deref g
    destination
  gain (Amp 0.6)
  destination

test6 :: Synth ()
test6 = buildSynth $ do
  gGain <- ref $ gain (Amp 0.4)
  oscillator Sine (Hz 440)
  g <- branch $ do
    deref gGain
    gain (Amp 0.5)
  h <- branch $ gain (Amp 0.7)
  _ <- merge [g, h]
  gain (Amp 0.6)
  destination

test7 :: Synth ()
test7 = buildSynth $ do
  oscillator Sine (Hz 440)
  _ <- mix $ replicate 3 $ gain (Amp 0.5)
  return ()

test8 :: Synth ()
test8 = buildSynth $ do
  g <- ref $ gain (Amp 0.5)
  setParamValue g "gain" 0.0 $ Sec 1.0
  
test9 :: Synth ()
test9 = buildSynth $ do
  oscillator Sine (Hz 440)
  diverge $ do
    gain (Amp 0.6)
    destination
  gain (Amp 0.7)
  
test10 :: Synth ()
test10 = buildSynth $ do
  oscillator Sine (Hz 440)
  g <- ref $ gain (Db $ -20)
  deref g -- TODO make a ref that also includes the node in addition to the ref
  diverge $ do -- lfo gain modulation
    oscillator Sine (Hz 2) -- osc.connect(g.gain)
    audioParamSink g "gain"
  destination

test11 :: Synth ()
test11 = buildSynth $ do
  oscillator Sine (Hz 400)
  g <- gain (Db $ -20)
  --mergeDiverge $ do
  do
    oscillator Sine (Hz 2)
    audioParam g "gain"
  destination
  
test12 :: Synth ()
test12 = buildSynth $ do
  oscillator Sine (Hz 440)
  b <- gain (Db $ -20)
  do
    filter
    destination
  do
    audioFrom b
    myAmazingDistortion
    destination
  -- destination error
  
  myAmazingSynth
  myAmazingSynth
  
  oscillator
  destination
  mergeDiverge $ oscillator >> destination

additive :: [(Double, Double)] -> SynthBuilder [Graph]
additive = mapM $ \(freq, amp) -> do
  oscillator Sine (Hz freq)
  gain (Amp amp)
  
mixM :: ()

myAmazingDistortion :: SynthBuilder Graph
myAmazingDistortion = do
  distortion
  filter
  distortion
    
    
    
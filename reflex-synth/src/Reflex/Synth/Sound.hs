module Reflex.Synth.Sound where

import Reflex.Synth.NodeSpec

data Sound =
  NoSound |
  Sound Source |
  GainSound Sound Double |  -- gain in dB
  FilteredSound Source Filter  |
  CompressedSound Sound Compressor |
  ProcessedSound Sound DSPEffect |
  WaveShapedSound Sound WaveShaper |
  ReverberatedSound Sound Buffer |
  OverlappedSound String [Sound] |
  DelayedSound Sound Double |
  TwoNotesSound Sound Double Sound deriving (Read,Show)
    -- String is sort of an unfortunately necessary identifier - so that if playing a sound of an indefinite length (such as a looped buffer) overlapped with other sounds, when you call 'stop' (Read,Show)

soundTwo = OverlappedSound "Test" [Sound (NodeSource (OscillatorNode ( Oscillator Sine 440 (-10))) (Just 2)), Sound ( NodeSource (BufferNode ( File "pinknoise.wav")) (Just 2))]

-- soundOne = OverlappedSound "Test" [Sound $ NodeSource (OscillatorNode $ Oscillator Sine 440 (-10)) (Just 2)]


createSound :: Sound -> IO WebAudioGraph
createSound (FilteredSound s f) = do
  sourceNode <- createGraph s
  filterNode <- createGraph f
  dest <- getDestination
  let graph = WebAudioGraph'' sourceNode (WebAudioGraph'' filterNode (WebAudioGraph dest))
  connectGraph graph


-- -- get duration of a sound. Nothing denotes that the sound will play indefinitely until the user hits stop.
-- getT :: Sound -> Maybe Double
-- getT (OverlappedSound identifier xs) = minimum $ fmap getT xs
-- getT a =
-- getT a = case (getSource a ) of
--   (NodeSource _ t) ->  t
--   otherwise -> Nothing

getT:: Sound -> Maybe Double
getT (Sound (NodeSource _ t)) = t
getT (GainSound s _) = getT s
getT (FilteredSound (NodeSource _ t) _) = t
getT (ProcessedSound s _) = getT s
getT (NoSound) = Nothing
getT (WaveShapedSound s _) = getT s
getT (ReverberatedSound s _) = getT s
getT (CompressedSound s _) = getT s
getT (DelayedSound s d) = fmap (+d) (getT s)
getT (TwoNotesSound n1 t n2) = do
  t1 <- getT n1
  let m = max t t1
  t2 <- getT n2
  return $ m + t2
getT (OverlappedSound _ xs) = minimum $ fmap getT xs

getSource:: Sound -> Source
getSource (Sound s) = s
getSource (GainSound s _) = getSource s
getSource (FilteredSound s _) = s
getSource (ProcessedSound s _) = getSource s
getSource (NoSound) = NodeSource SilentNode $ Just 2
getSource (WaveShapedSound s _) = getSource s
getSource (ReverberatedSound s _) = getSource s
getSource (CompressedSound s _) = getSource s
getSource (DelayedSound s _) = getSource s
getSource (OverlappedSound _ _) = error "cannot get 'source' of an OverlappedSound"

module Reflex.Synth.Sound where

import Reflex.Synth.NodeSpec
import GHCJS.Marshal.Pure (pToJSVal)
import Reflex.Synth.Foreign as F

import Control.Monad (liftM)


data Sound =
  NoSound |
  Sound Source |
  GainSound Sound Double |  -- gain in dB
  FilteredSound Source Filter  |
  CompressedSound Sound Compressor |
  ProcessedSound Sound DSPEffect |
  WaveShapedSound Sound WaveShaper |
  ReverberatedSound Sound Buffer |
  OverlappedSound String [Sound] EndTime |
  DelayedSound Sound Double |
  TwoNotesSound Sound Double Sound deriving (Read,Show)
    -- String is sort of an unfortunately necessary identifier - so that if playing a sound of an indefinite length (such as a looped buffer) overlapped with other sounds, when you call 'stop' (Read,Show)

data EndTime = Max | Min | OnBufferEnd String deriving (Read, Show, Eq)



-- soundOne = OverlappedSound "Test" [Sound $ NodeSource (OscillatorNode $ Oscillator Sine 440 (-10)) (Just 2)]

--
-- createSound :: Sound -> IO WebAudioGraph
-- createSound (FilteredSound s f) = do
--   sourceNode <- createGraph s
--   filterNode <- createGraph f
--   dest <- getDestination
--   let graph = WebAudioGraph'' sourceNode (WebAudioGraph'' filterNode (WebAudioGraph dest))
--   connectGraph graph


-- -- get duration of a sound. Nothing denotes that the sound will play indefinitely until the user hits stop.
-- getT :: Sound -> Maybe Double
-- getT (OverlappedSound identifier xs) = minimum $ fmap getT xs
-- getT a =
-- getT a = case (getSource a ) of
--   (NodeSource _ t) ->  t
--   otherwise -> Nothing


-- needs to be IO bc. need to get buffer duration for overlapped sounds that should end when buffer ends
getT:: Sound -> IO (Maybe Double)
getT (Sound (NodeSource _ t)) = return t
getT (GainSound s _) = getT s
getT (FilteredSound (NodeSource _ t) _) = return  t
getT (ProcessedSound s _) = getT s
getT (NoSound) = return  Nothing
getT (WaveShapedSound s _) = getT s
getT (ReverberatedSound s _) = getT s
getT (CompressedSound s _) = getT s
getT (DelayedSound s d) = liftM (fmap (+d)) (getT s)
getT (OverlappedSound _ xs Max) = liftM maximum $ sequence  $ fmap getT xs
getT (OverlappedSound _ xs Min) = liftM  minimum $ sequence   $ fmap getT xs
getT (OverlappedSound _ xs (OnBufferEnd s)) = do
  buf <- getBufferByID (pToJSVal s)
  t <- F.getBufferDuration buf
  putStrLn ("Buffer duration time:   "++show t)
  return (Just t)



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
getSource (OverlappedSound _ _ _) = error "cannot get 'source' of an OverlappedSound"

{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Reflex.Synth.Spec where

import GHCJS.Marshal.Pure

data SourceNodeSpec where
  Silent :: SourceNodeSpec
  Oscillator :: (FrequencyInHz f) => OscillatorType -> f -> SourceNodeSpec
  -- TODO | Buffer BufferSrc
deriving instance Show SourceNodeSpec

data SourceSinkNodeSpec where
  Filter :: FilterSpec -> SourceSinkNodeSpec
  -- TODO | Convolver Buffer normalize :: Boolean
  Delay :: (TimeInSec t) => t -> SourceSinkNodeSpec
  Compressor :: (AmplitudeInDb t, AmplitudeInDb k, AmplitudeInDb r, AmplitudeInDb d, TimeInSec a, TimeInSec e) => { 
      threshold :: t, knee :: k, ratio :: r, reduction :: d, attack :: a, release :: e
    } -> SourceSinkNodeSpec
  Gain :: (AmplitudeInAmp a) => a -> SourceSinkNodeSpec
  WaveShaper :: [Double] -> OversampleAmount -> SourceSinkNodeSpec
  -- DistortAt Amplitude
deriving instance Show SourceSinkNodeSpec

data SinkNodeSpec
  = Destination
  deriving (Show)

data OscillatorType 
  = Sine 
  | Square 
  | Sawtooth 
  | Triangle 
  deriving (Show, Eq)

instance PToJSVal OscillatorType where
  pToJSVal Sine = "sine"
  pToJSVal Square = "square"
  pToJSVal Sawtooth = "sawtooth"
  pToJSVal Triangle = "triangle"

data FilterSpec where
  -- frequency -> Q -> gain
  LowPass :: (FrequencyInHz f) => f -> Double -> FilterSpec
  HighPass :: (FrequencyInHz f) => f -> Double -> FilterSpec
  BandPass :: (FrequencyInHz f) => f -> Double -> FilterSpec
  LowShelf :: (FrequencyInHz f, AmplitudeInDb g) => f -> g -> FilterSpec
  HighShelf :: (FrequencyInHz f, AmplitudeInDb g) => f -> g -> FilterSpec
  Peaking :: (FrequencyInHz f, AmplitudeInDb g) => f -> Double -> g -> FilterSpec
  Notch :: (FrequencyInHz f) => f -> Double -> FilterSpec
  AllPass :: (FrequencyInHz f) => f -> Double -> FilterSpec
  -- | IIR [Double] [Double] feedforward feedback
  deriving (Show)

data OversampleAmount
  = None
  | Times2
  | Times4
  deriving (Show)
  
instance PToJSVal OversampleAmount where
  pToJSVal None = "none"
  pToJSVal Times2 = "x2"
  pToJSVal Times4 = "x4"

data Amplitude
  = Amp Double
  | Db Double
  deriving (Show, Eq)

class AmplitudeInAmp a where
  inAmp :: a -> Double

instance Show (AmplitudeInAmp a) where
  show = show . inAmp

instance AmplitudeInAmp Amplitude where
  inAmp (Amp a) = a
  inAmp (Db db) = 10.0 ** (db / 20.0)

class AmplitudeInDb a where
  inDb :: a -> Double

instance Show (AmplitudeInDb a) where
  show x = (show $ inDb x) ++ "dB"

instance AmplitudeInDb Amplitude where
  inDb (Amp a) = 20.0 * (logBase 10 a)
  inDb (Db db) = db

data Frequency
  = Hz Double
  | Midi Double
  deriving (Show, Eq)

class FrequencyInHz a where
  inHz :: a -> Double

instance Show (FrequencyInHz a) where
  show x = (show $ inHz x) ++ "Hz"

instance FrequencyInHz Frequency where
  inHz (Hz hz) = hz
  inHz (Midi n) = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

class FrequencyInMidi a where
  inMidi :: a -> Double

instance Show (FrequencyInMidi a) where
  show x = (show $ inMidi x) ++ "midi"

instance FrequencyInMidi Frequency where
  inMidi (Hz hz) = 69.0 + 12.0 * (logBase 2 (hz / 440.0))
  inMidi (Midi n) = n

data Time
  = Sec Double
  | Millis Double
  deriving (Show, Eq)

class TimeInSeconds a where
  inSec :: a -> Double

instance Show (TimeInSeconds a) where
  show x = (show $ inSec x) ++ "s"

instance TimeInSeconds Time where
  inSec (Sec s) = s
  inSec (Millis ms) = ms / 1000.0

class TimeInMillis a where
  inMillis :: a -> Double 

instance Show (TimeInMillis a) where
  show x = (show $ inMillis x) ++ "ms"

instance TimeInMillis Time where
  inMillis (Sec s) = s * 1000.0
  inMillis (Millis ms) = ms

instance Num Time where
  t1 + t2 = Millis $ (inMillis t1) + (inMillis t2)
  t1 - t2 = Millis $ (inMillis t1) - (inMillis t2)
  t1 * t2 = Millis $ (inMillis t1) * (inMillis t2)
  abs (Millis ms) = Millis $ abs ms
  abs (Sec s) = Sec $ abs s
  signum x = 
    case inMillis x of 
      y | y < 0 -> -1
        | y == 0 -> 0
        | otherwise -> 1
  fromInteger ms = Millis $ fromIntegral ms

--data PlaybackParam = PlaybackParam{
--  start::Double,    -- portion through the buffer that playback starts
--  end::Double,
--  loop::Bool
--} deriving (Read, Show, Eq)
--
--getPlaybackParam :: Source -> Maybe PlaybackParam
--getPlaybackParam (NodeSource (BufferNode (LoadedFile _ x)) _) = Just x
--getPlaybackParam _ = Nothing
--
--isLoadedFile :: Source -> Bool
--isLoadedFile (NodeSource (BufferNode (LoadedFile _ _)) _) = True
--isLoadedFile _ = False

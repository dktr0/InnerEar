{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Reflex.Synth.Spec where

data SourceNodeSpec where
  Silent :: SourceNodeSpec
  Oscillator :: (FrequencyInHz f, Show f) => OscillatorType -> f -> SourceNodeSpec
  -- TODO | Buffer BufferSrc
deriving instance Show SourceNodeSpec

data SourceSinkNodeSpec
  = Filter FilterSpec
  -- TODO | Convolver Buffer normalize :: Boolean
  | Delay Time
  | Compressor CompressorSpec
  | Gain Amplitude
  | WaveShaper [Double] OversampleAmount
  | DistortAt Amplitude -- TODO replace with an existing node to remove scriptnode dependency
  deriving (Show)

data SinkNodeSpec
  = Destination
  deriving (Show)

data OscillatorType 
  = Sine 
  | Square 
  | Sawtooth 
  | Triangle 
  deriving (Show, Read, Eq)

data FilterSpec
  = LowPass Frequency Double
  | HighPass Frequency Double
  | BandPass Frequency Double
  | LowShelf Frequency Amplitude
  | HighShelf Frequency Amplitude
  | Peaking Frequency Double Amplitude
  | Notch Frequency Double
  | AllPass Frequency Double
  -- | IIR [Double] [Double] feedforward feedback
  deriving (Show)

data CompressorSpec = CompressorSpec { 
  threshold :: Amplitude, 
  knee :: Amplitude,
  ratio :: Amplitude,
  reduction :: Double,
  attack :: Time, 
  release :: Time
} deriving (Show)

data OversampleAmount
  = None
  | Times2
  | Times4
  deriving (Show)

data Amplitude
  = Amp Double
  | Db Double
  deriving (Show, Eq, Ord)

class AmplitudeInAmp a where
  inAmp :: a -> Double

instance AmplitudeInAmp Amplitude where
  inAmp (Amp a) = a
  inAmp (Db db) = 10.0 ** (db / 20.0)

class AmplitudeInDb a where
  inDb :: a -> Double

instance AmplitudeInDb Amplitude where
  inDb (Amp a) = 20.0 * (logBase 10 a)
  inDb (Db db) = db

data Frequency
  = Hz Double
  | Midi Double
  deriving (Show, Eq, Ord)

class FrequencyInHz a where
  inHz :: a -> Double
  
instance FrequencyInHz Frequency where
  inHz (Hz hz) = hz
  inHz (Midi n) = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

class FrequencyInMidi a where
  inMidi :: a -> Double

instance FrequencyInMidi Frequency where
  inMidi (Hz hz) = 69.0 + 12.0 * (logBase 2 (hz / 440.0))
  inMidi (Midi n) = n

data Time
  = Sec Double
  | Millis Double
  deriving (Show, Eq, Ord)

class TimeInSeconds a where
  inSec :: a -> Double
  
instance TimeInSeconds Time where
  inSec (Sec s) = s
  inSec (Millis ms) = ms / 1000.0

class TimeInMillis a where
  inMillis :: a -> Double 

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

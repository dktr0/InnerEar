module Reflex.Synth.Spec where

data SourceNodeSpec
  = Silent
  | Oscillator OscillatorType Frequency
  -- TODO | Buffer BufferSrc
  deriving (Show)

data NodeSpec
  = Filter FilterSpec
  -- TODO | Convolver Buffer normalize :: Boolean
  | Delay Time
  | Compressor { 
      threshold :: Amplitude, 
      knee :: Amplitude,
      ratio :: Amplitude,
      reduction :: Double,
      attack :: Time, 
      release :: Time
    }
  | Gain Amplitude
  | WaveShaper [Double] OversampleAmount
  | DistortAt Amplitude -- TODO replace with an existing node to remove scriptnode dependency
  deriving (Show)

data SinkNodeSpec
  = Destination
  deriving (Show)

--envelope :: Double ... -> Synth ()
--envelope a d s st r =
--  oscillator Sine
--  g <- ref $ gain 0
--  change (gain g) Constant (dbamp 0) 0
--  change Linear 1 a
--  change Exponential s $ a + d
--  change Constant s $ a + d + st
--  change Linear 0 $ a + d + st + r
--  stop g
--
-- cs = [Change 
-- $0 = create...
-- osc = ..
-- osc.connect(g)

data Amplitude
  = Amp Double
  | Db Double
  deriving (Show, Eq, Ord)
  
getInAmp :: Amplitude -> Double
getInAmp (Amp a) = a
getInAmp (Db db) = 10.0 ** (db / 20.0)

getInDb :: Amplitude -> Double
getInDb (Amp a) = 20.0 * (logBase 10 a)
getInDb (Db db) = db

data Frequency
  = Hz Double
  | Midi Double
  deriving (Show, Eq, Ord)

getInHz :: Frequency -> Double
getInHz (Hz hz) = hz
getInHz (Midi n) = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

getInMidi :: Frequency -> Double
getInMidi (Hz hz) = 69.0 + 12.0 * (logBase 2 (hz / 440.0))
getInMidi (Midi n) = n

data Time
  = Sec Double
  | Millis Double
  deriving (Show, Eq, Ord)

getInSec :: Time -> Double
getInSec (Sec s) = s
getInSec (Millis ms) = ms / 1000.0

getInMillis :: Time -> Double 
getInMillis (Sec s) = s * 1000.0
getInMillis (Millis ms) = ms

instance Num Time where
  t1 + t2 = Millis $ (getInMillis t1) + (getInMillis t2)
  t1 - t2 = Millis $ (getInMillis t1) - (getInMillis t2)
  t1 * t2 = Millis $ (getInMillis t1) * (getInMillis t2)
  abs (Millis ms) = Millis $ abs ms
  abs (Sec s) = Sec $ abs s
  signum x = 
    case getInMillis x of 
      y | y < 0 -> -1
        | y == 0 -> 0
        | otherwise -> 1
  fromInteger ms = Millis $ fromIntegral ms

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

data OversampleAmount
  = None
  | Times2
  | Times4
  deriving (Show)

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

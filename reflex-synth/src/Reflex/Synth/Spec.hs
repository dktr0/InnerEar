{-# LANGUAGE OverloadedStrings #-}

module Reflex.Synth.Spec where

import GHCJS.Marshal.Pure
import GHCJS.Types
import Data.Monoid
import Reflex.Synth.AudioRoutingGraph(Float32Array)

data SourceNodeSpec
  = Silent
  | Oscillator OscillatorType Frequency
  -- | Buffer (Either Float32Array FloatArraySpec) Double Double Bool
  | Buffer SoundFile PlaybackParam
  deriving (Show)

data SoundFile = Ours String | Theirs String

data Buffer = Buffer [Float32ArraySpec] Double

data SourceSinkNodeSpec
  = Filter FilterSpec
  -- Convolver buffer normalize
  | Convolver (Either Float32Array FloatArraySpec) Bool
  | Delay Time
  -- Compressor threshold knee ratio reduction attack release
  | Compressor Amplitude Amplitude Amplitude Gain Time Time
  | Gain Gain
  | WaveShaper (Either Float32Array FloatArraySpec) OversampleAmount
  | DistortAt Amplitude
  deriving (Show)

data SinkNodeSpec
  = Destination
  deriving (Show)

data OscillatorType 
  = Sine
  | Square
  | Sawtooth
  | Triangle 
  deriving (Show, Eq)

newtype AudioBuffer = AudioBuffer JSVal
-- data BufferSrc s = Uploaded String | Local String deriving (Show, Eq) -- TODO is this the best way to handle loaded files?...

data PlaybackParam = PlaybackParam Double Double Bool deriving (Show, Eq)

instance PToJSVal OscillatorType where
  pToJSVal Sine = jsval ("sine" :: JSString)
  pToJSVal Square = jsval ("square" :: JSString)
  pToJSVal Sawtooth = jsval ("sawtooth" :: JSString)
  pToJSVal Triangle = jsval ("triangle" :: JSString)

data FloatArraySpec
  = EmptyArray
  | Const Int Double FloatArraySpec
  | Segment [Double] FloatArraySpec
  | Repeated Int [Double] FloatArraySpec
  deriving (Show)

instance Monoid FloatArraySpec where
  mempty = EmptyArray
  mappend EmptyArray x = x
  mappend x EmptyArray = x
  mappend (Const i x tl) y = Const i x $ mappend tl y
  mappend (Segment xs tl) y = Segment xs $ mappend tl y
  mappend (Repeated i xs tl) y = Repeated i xs $ mappend tl y

arraySpecMap :: (Double -> Double) -> FloatArraySpec -> FloatArraySpec
arraySpecMap f EmptyArray = EmptyArray
arraySpecMap f (Const i x tl)= Const i (f x) $ arraySpecMap f tl
arraySpecMap f (Segment xs tl) = Segment (fmap f xs) $ arraySpecMap f tl
arraySpecMap f (Repeated i xs tl) = Repeated i (fmap f xs) $ arraySpecMap f tl

arraySpecSize :: FloatArraySpec -> Int
arraySpecSize EmptyArray = 0
arraySpecSize (Const i _ tl) = i + arraySpecSize tl
arraySpecSize (Segment xs tl) = (length xs) + arraySpecSize tl
arraySpecSize (Repeated i xs tl) = (i * (length xs)) + arraySpecSize tl

listToArraySpec :: [Double] -> FloatArraySpec
listToArraySpec xs = Segment xs EmptyArray

data FilterSpec
  = LowPass Frequency Double
  | HighPass Frequency Double
  | BandPass Frequency Double
  | LowShelf Frequency Gain
  | HighShelf Frequency Gain
  | Peaking Frequency Double Gain
  | Notch Frequency Double
  | AllPass Frequency Double
  -- | IIR [Double] [Double] feedforward feedback
  deriving (Show)

data OversampleAmount
  = None
  | Times2
  | Times4
  deriving (Show)

instance PToJSVal OversampleAmount where
  pToJSVal None = jsval ("none" :: JSString)
  pToJSVal Times2 = jsval ("x2" :: JSString)
  pToJSVal Times4 = jsval ("x4" :: JSString)

data Amplitude
  = Amp Double
  | Db Double
  deriving (Show)

type Gain = Amplitude

inAmp :: Amplitude -> Double
inAmp (Amp a) = a
inAmp (Db db) = 10.0 ** (db / 20.0)

inDb :: Amplitude -> Double
inDb (Amp a) = 20.0 * (logBase 10 a)
inDb (Db db) = db

data Frequency
  = Hz Double
  | Midi Double
  deriving (Show)

inHz :: Frequency -> Double
inHz (Hz hz) = hz
inHz (Midi n) = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

inMidi :: Frequency -> Double
inMidi (Hz hz) = 69.0 + 12.0 * (logBase 2 (hz / 440.0))
inMidi (Midi n) = n

data Time
  = Sec Double
  | Millis Double
  deriving (Show)

inSec :: Time -> Double
inSec (Sec s) = s
inSec (Millis ms) = ms / 1000.0

inMillis :: Time -> Double
inMillis (Sec s) = s * 1000.0
inMillis (Millis ms) = ms

instance Eq Time where
  t1 == t2 = inMillis t1 == inMillis t2

instance Ord Time where
  t1 <= t2 = inMillis t1 <= inMillis t2

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

-- TODO AudioBuffer spec with float32arrays and maybe a sample rate?

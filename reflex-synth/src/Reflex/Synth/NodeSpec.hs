module Reflex.Synth.NodeSpec where

data FilterType = Peaking | Lowpass | Highpass | Notch | Bandpass | Lowshelf | Highshelf | Allpass deriving (Show,Read,Eq)

data NoiseType = White | Pink | Brownian

data NodeSpec =
  SilentNode |
  FilterNode Filter |
  GainNode Double |
  Destination |
  AdditiveNode [NodeSpec] |
  OscillatorNode Oscillator |
  BufferNode Buffer |
  MediaNode String |
  CompressorNode  Compressor |
  WaveShaperNode WaveShaper|
  ScriptProcessorNode DSPEffect |
  DelayNode Double |
  EnvelopeNode Envelope |
  ConvolverNode Buffer deriving(Read,Show,Eq)


data Envelope = Custom {curve::[Double], duration::Double} deriving (Show, Eq, Read)


data DSPEffect = DistortAtDb Double deriving (Read, Show, Eq)

data WaveShaper = ClipAt Double deriving (Show, Read, Eq)

data Compressor = Compressor {threshold::Double, knee::Double, ratio::Double, attack::Double, release::Double} deriving (Show, Read, Eq)

data Filter = NoFilter | Filter FilterType Double Double Double deriving (Read,Show,Eq) -- Freq, q, db

data OscillatorType = Sawtooth | Sine | Square | Triangle deriving (Show, Read,Eq)

data Oscillator = Oscillator OscillatorType Double Double | Oscillator' OscillatorType Envelope Double deriving (Read,Show,Eq) --double params are freq and gain (in dB) (respectively)

-- data Oscillator = Oscillator

data PlaybackParam = PlaybackParam{
  start::Double,    -- portion through the buffer that playback starts
  end::Double,
  loop::Bool
} deriving (Read, Show, Eq)

data Buffer = File String | LoadedFile String PlaybackParam deriving (Read,Show,Eq)

data Source = NodeSource NodeSpec (Maybe Double)  deriving (Show, Eq, Read)

getPlaybackParam :: Source -> Maybe PlaybackParam
getPlaybackParam (NodeSource (BufferNode (LoadedFile _ x)) _) = Just x
getPlaybackParam _ = Nothing

isLoadedFile :: Source -> Bool
isLoadedFile (NodeSource (BufferNode (LoadedFile _ _)) _) = True
isLoadedFile _ = False

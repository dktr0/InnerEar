module InnerEar.Types.Sound (
  SoundSourceConfigOption(..),
  SoundSource(..),
  SoundSourceConfig(..),
) where

import Reflex.Synth.Buffer
import Reflex.Synth.Spec

data SoundSourceConfigOption
  = Spec SourceNodeSpec
  | Resource String
  | UserProvidedResource

data SoundSource
  = SourceLoading
  | SourceLoaded SourceNodeSpec
  | SourceError String
  | SourceUnderSpecified

data SoundSourceConfig = SoundSourceConfig {
  source :: SoundSource,
  playbackRange :: (Double, Double),
  shouldLoop :: Bool
}

module InnerEar.Types.Sound (
  SoundSourceConfigOption(..),
  SoundSource(..),
  SoundSourceConfig(..),
) where

import Sound.MusicW


data SoundSourceConfigOption
  = Spec SourceNodeSpec (Maybe Time)
  | Resource String (Maybe Time)
  | UserProvidedResource

data SoundSource
  = SourceLoading
  | SourceLoaded SourceNodeSpec (Maybe Time)
  | SourceError String
  | SourceUnderSpecified

data SoundSourceConfig = SoundSourceConfig {
  source :: SoundSource,
  playbackRange :: (Double, Double),
  shouldLoop :: Bool
}

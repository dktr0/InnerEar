module InnerEar.Types.Sound where

import Reflex.Synth.Spec

data SoundSourceConfigOption
  = Spec SourceNodeSpec
  | Resource String
  | UserProvidedResource
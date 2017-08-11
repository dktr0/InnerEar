module InnerEar.Types.Sound where

import InnerEar.Types.Utility


data Source = PinkNoise Duration

data Sound = NoSynth | FilteredSound Source Filter




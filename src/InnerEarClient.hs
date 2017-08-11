module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Client

main :: IO ()
main = createAudioContext >> mainWidget clientWidget

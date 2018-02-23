module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Types
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main = createAudioContext >> startSilentNode >> mainWidget clientWidget

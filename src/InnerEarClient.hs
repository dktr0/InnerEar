module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main = createAudioContext >> mainWidget clientWidget

module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main = do
	inst <- instnatiateSynth $ buildSynth $ silent >> destination
	startSynth (Sec 0) inst
	globalBuffers <- loadGlobalResources
	mainWidget $ clientWidget globalBuffers

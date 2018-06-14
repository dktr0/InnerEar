module Main where

import Data.Map

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main = do
	globalBuffers <- loadGlobalResources
	mainWidget $ clientWidget globalBuffers

loadGlobalResources :: IO (Map String AudioBuffer)
loadGlobalResources = return empty
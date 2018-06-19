module Main where

import Data.Map

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
-- module Reflex.Synth.Buffer -- maybe get rid of this import if possible
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main =	mainWidget $ do
		globalBuffers <- loadGlobalResources
		widgetHold (return ()) $ fmap clientWidget globalBuffers
		return ()

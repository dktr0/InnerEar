module Main where

import Data.Map

import Reflex
import Reflex.Dom
import Sound.MusicW -- is this import needed?
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main =	mainWidget $ do
		globalBuffers <- loadGlobalResources
		widgetHold (return ()) $ fmap clientWidget globalBuffers
		return ()

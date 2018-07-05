module Main where

import Data.Map

import Reflex
import Reflex.Dom
import InnerEar.Widgets.Client

import InnerEar.Exercises.MultipleChoice

main :: IO ()
main =	mainWidget $ do
  globalBuffers <- loadGlobalResources
  globalBuffers' <- holdDyn empty globalBuffers
  clientWidget globalBuffers'

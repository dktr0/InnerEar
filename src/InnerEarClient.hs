{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Navigation

main :: IO ()
main = do
  createAudioContext
  mainWidget $ el "div" $ do
  							navigationWidget never
  							return ()

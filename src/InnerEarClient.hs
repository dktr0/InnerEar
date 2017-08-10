{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Main where

import Reflex
import Reflex.Dom
import Reflex.Synth.Synth
import InnerEar.Widgets.Client

main :: IO ()
main = do
  createAudioContext
  mainWidget $ el "div" $ do
    clientWidget never
    return ()

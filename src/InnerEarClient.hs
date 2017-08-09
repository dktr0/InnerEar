{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Main where

import Reflex
import Reflex.Dom

import InnerEar.Widgets.Navigation

main :: IO ()
main = mainWidget $ el "div" $ do
  navigationWidget never
  return ()

{-
anEarTrainingExercise :: MonadWidget t m
  => [Record] -- selected records about user choices, actions, results
  -> m ( Event t Navigation, -- received by container to exchange this widget for another one
         Event t Sound, -- received by container and performed by web audio infrastructure
         Event t [Record], -- user records to be accumulated by container and server )
-}

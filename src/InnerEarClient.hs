{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Main where

import Reflex
import Reflex.Dom

import InnerEar.Types.Record

main :: IO ()
main = mainWidget $ el "div" $ text "Inner Ear"

{-
anEarTrainingExercise :: MonadWidget t m
  => [Record] -- selected records about user choices, actions, results
  -> m ( Event t Navigation, -- received by container to exchange this widget for another one
         Event t Sound, -- received by container and performed by web audio infrastructure
         Event t [Record], -- user records to be accumulated by container and server )
-}

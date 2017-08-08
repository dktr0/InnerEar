{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ text "Inner Ear"

{-
anEarTrainingExercise :: MonadWidget t m
  => [Data] -- selected information about user choices, actions, results
  -> m ( Event t Navigation, -- received by container to exchange this widget for another one
         Event t Sound, -- received by container and performed by web audio infrastructure
         Event t [Data], -- a single timepoint of user data, to be accumulated )
-}

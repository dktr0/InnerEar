{-# LANGUAGE DeriveDataTypeable, RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation (navigationWidget) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom
import Data.Time.Clock (getCurrentTime)
import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.Data
import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.CreateUser
import InnerEar.Widgets.Sound
import InnerEar.Widgets.Test
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise
import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Exercise

import InnerEar.Exercises.ThresholdOfSilence
import InnerEar.Exercises.HarmonicDistortion
import InnerEar.Exercises.BoostOrCut
import InnerEar.Exercises.FiveBandBoostCut
import InnerEar.Exercises.TenBandBoostCut
import InnerEar.Exercises.AddedWhiteNoise
import InnerEar.Exercises.RT60
import InnerEar.Exercises.Compression
import InnerEar.Exercises.LeftRightCentre

data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage ExerciseId |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> Dynamic t Bool -> m (Event t Request,Event t Sound)
navigationWidget responses areTheyAuthenticated = elClass "div" "mainBody" $ mdo
  let initialPage = navigationPage responses areTheyAuthenticated SplashPage
  let rebuild = fmap (navigationPage responses areTheyAuthenticated) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn (\(x,_,_) -> x) w
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_) -> x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x) -> x) w
  return (requests,sounds)

includedExercises = [
  ThresholdOfSilence,
  HarmonicDistortion,
  BoostOrCut,
  FiveBandBoostCut
--  AddedWhiteNoise,
--  RT60,
--  Compression,
--  LeftRightCentre
  ]

buttonForExercise :: MonadWidget t m => ExerciseId -> m (Event t Navigation)
buttonForExercise x = elClass "div" "navButton" $ do
  y <- button (showExerciseTitle x)
  return $ fmap (\_ -> ExercisePage x) y

navigationPage :: MonadWidget t m => Event t [Response] -> Dynamic t Bool -> Navigation -> m (Event t Request,Event t Sound,Event t Navigation)

navigationPage responses areTheyAuthenticated SplashPage = elClass "div" "nav" $ do
  elClass "div" "explanation" $
    text "Welcome to Inner Ear! Select an ear-training exercise from the list below. If you are doing this is part of a requirement for a class, please make sure you are logged in first (at the top right)."
  b <- mapM buttonForExercise includedExercises
  -- c <- liftM (TestPage <$)  $ elClass "div" "navButton" $ button "Test"
  -- d <- liftM (TestSoundPage <$) $ elClass "div" "navButton" $ button "Test Sound"
  let navEvents = leftmost b
  return (never,never,navEvents)

navigationPage responses areTheyAuthenticated CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage responses areTheyAuthenticated (ExercisePage ThresholdOfSilence) =
  runExerciseForNavigationPage thresholdOfSilenceExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage HarmonicDistortion) =
  runExerciseForNavigationPage harmonicDistortionExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage BoostOrCut) =
  runExerciseForNavigationPage boostOrCutExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage FiveBandBoostCut) =
  runExerciseForNavigationPage fiveBandBoostCutExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage TenBandBoostCut) =
  runExerciseForNavigationPage tenBandBoostCutExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage AddedWhiteNoise) =
  runExerciseForNavigationPage addedWhiteNoiseExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage RT60) =
  runExerciseForNavigationPage rt60Exercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage Compression) =
  runExerciseForNavigationPage compressionExercise responses areTheyAuthenticated
navigationPage responses areTheyAuthenticated (ExercisePage LeftRightCentre) =
  runExerciseForNavigationPage leftRightCentreExercise responses areTheyAuthenticated

navigationPage responses areTheyAuthenticated TestPage = do
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses areTheyAuthenticated TestSoundPage = do
  (requests,sounds,navUnit) <- testSoundWidget responses
  return (requests,sounds,SplashPage <$ navUnit)


runExerciseForNavigationPage :: (MonadWidget t m, Data c, Data q, Data a, Data e, Show c, Show q, Show a, Show e)
  => Exercise t m c q a e
  -> Event t [Response] -> Dynamic t Bool
  -> m (Event t Request,Event t Sound,Event t Navigation)
runExerciseForNavigationPage ex responses areTheyAuthenticated = do
  (newData,sounds,navUnit) <- runExercise ex
  let newData' = gate (current areTheyAuthenticated) newData
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData'
  let newRequest = PostPoint <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

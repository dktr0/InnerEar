{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation (navigationWidget) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom
import Data.Time.Clock (getCurrentTime)

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

data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage ExerciseId |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request,Event t Sound)
navigationWidget responses = elClass "div" "mainBody" $ mdo
  let initialPage = navigationPage responses SplashPage
  let rebuild = fmap (navigationPage responses) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn (\(x,_,_) -> x) w
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_) -> x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x) -> x) w
  return (requests,sounds)

navigationPage :: MonadWidget t m => Event t [Response] -> Navigation -> m (Event t Request,Event t Sound,Event t Navigation)

navigationPage responses SplashPage = elClass "div" "nav" $ do
  elClass "div" "explanation" $
    text "Welcome to Inner Ear! Select an ear-training exercise from the list below. If you are doing this is part of a requirement for a class, please make sure you are logged in first (at the top right)."
  b0 <- liftM (ExercisePage ThresholdOfSilence <$)  $ elClass "div" "navButton" $ button "Threshold Of Silence"
  b1 <- liftM (ExercisePage HarmonicDistortion <$)  $ elClass "div" "navButton" $ button "Harmonic Distortion"
  b2 <- liftM (ExercisePage BoostOrCut <$)  $ elClass "div" "navButton" $ button "Boost Or Cut (Gain)"
  b3 <- liftM (ExercisePage FiveBandBoostCut <$)  $ elClass "div" "navButton" $ button "Five Band Boost or Cut (Filters)"
  b4 <- liftM (ExercisePage TenBandBoostCut <$)  $ elClass "div" "navButton" $ button "Ten Band Boost or Cut (Filters)"
  c <- liftM (TestPage <$)  $ elClass "div" "navButton" $ button "Test"
  d <- liftM (TestSoundPage <$) $ elClass "div" "navButton" $ button "Test Sound"
  let navEvents = leftmost [b0,b1,b2,b3,b4,c,d]
  return (never,never,navEvents)

navigationPage responses CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage responses (ExercisePage ThresholdOfSilence) = runExerciseForNavigationPage thresholdOfSilenceExercise
navigationPage responses (ExercisePage HarmonicDistortion) = runExerciseForNavigationPage harmonicDistortionExercise
navigationPage responses (ExercisePage BoostOrCut) = runExerciseForNavigationPage boostOrCutExercise
navigationPage responses (ExercisePage FiveBandBoostCut) = runExerciseForNavigationPage fiveBandBoostCutExercise
navigationPage responses (ExercisePage TenBandBoostCut) = runExerciseForNavigationPage tenBandBoostCutExercise

navigationPage responses TestPage = do
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses TestSoundPage = do
  (requests,sounds,navUnit) <- testSoundWidget responses
  return (requests,sounds,SplashPage <$ navUnit)


runExerciseForNavigationPage :: (MonadWidget t m, Show c, Show q, Show a, Show e)
  => Exercise t m c q a e
  -> m (Event t Request,Event t Sound,Event t Navigation)
runExerciseForNavigationPage ex = do
  (newData,sounds,navUnit) <- runExercise ex
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData
  let newRequest = PostRecord <$> Record "placeholderHandle" <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

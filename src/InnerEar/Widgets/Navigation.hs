{-# LANGUAGE DeriveDataTypeable, RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation (navigationWidget) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom
import Data.Time.Clock (getCurrentTime)
import Text.JSON
import Text.JSON.Generic
import Data.Maybe

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
import InnerEar.Widgets.DynSvg
import InnerEar.Widgets.Utility
import InnerEar.Types.User
import InnerEar.Widgets.UserList
import InnerEar.Widgets.UserPage
import InnerEar.Types.Handle

import InnerEar.Exercises.ThresholdOfSilence
import InnerEar.Exercises.HarmonicDistortion
import InnerEar.Exercises.BoostOrCut
import InnerEar.Exercises.FiveBandBoostCut
import InnerEar.Exercises.TenBandBoostCut
import InnerEar.Exercises.AddedWhiteNoise
import InnerEar.Exercises.Compression
import InnerEar.Exercises.OddEvenAll
import InnerEar.Exercises.SpectralShape
import InnerEar.Exercises.Intervals1
import InnerEar.Exercises.FrequencyEnvelope

data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage ExerciseId |
  AdminPage |
  UserListPage |
  UserPage Handle |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request,Event t Sound)
navigationWidget responses currentRole = elClass "div" "mainBody" $ mdo
  let initialPage = navigationPage responses currentRole SplashPage
  let rebuild = fmap (navigationPage responses currentRole) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn (\(x,_,_) -> x) w
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_) -> x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x) -> x) w
  return (requests,sounds)

includedExercises = [
  ThresholdOfSilence,
  HarmonicDistortion,
  BoostOrCut,
  FiveBandBoostCut,
  TenBandBoostCut,
  AddedWhiteNoise,
  Compression,
  OddEvenAll,
  SpectralShape,
  Intervals1,
  FrequencyEnvelope
  ]

buttonForExercise :: MonadWidget t m => ExerciseId -> m (Event t Navigation)
buttonForExercise x = elClass "div" "navButton" $ do
  y <- button (showExerciseTitle x)
  return $ fmap (\_ -> ExercisePage x) y

navigationPage :: MonadWidget t m => Event t [Response] -> Dynamic t (Maybe Role) -> Navigation -> m (Event t Request,Event t Sound,Event t Navigation)

navigationPage responses currentRole SplashPage = elClass "div" "nav" $ do
  elClass "div" "explanation" $
    text "Welcome to Inner Ear! Select an ear-training exercise from the list below. If you are doing this is part of a requirement for a class, please make sure you are logged in first (at the top right)."
  b <- mapM buttonForExercise includedExercises
  isAdmin <- mapDyn (==Just Administrator) currentRole
  a <- (AdminPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Administration")
  c <- (TestPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Test")
  d <- (TestSoundPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Test Sound")
  let navEvents = leftmost (a:c:d:b)
  return (never,never,navEvents)

navigationPage responses currentRole CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage responses currentRole (ExercisePage ThresholdOfSilence) =
  runExerciseForNavigationPage thresholdOfSilenceExercise responses currentRole
navigationPage responses currentRole (ExercisePage HarmonicDistortion) =
  runExerciseForNavigationPage harmonicDistortionExercise responses currentRole
navigationPage responses currentRole (ExercisePage BoostOrCut) =
  runExerciseForNavigationPage boostOrCutExercise responses currentRole
navigationPage responses currentRole (ExercisePage FiveBandBoostCut) =
  runExerciseForNavigationPage fiveBandBoostCutExercise responses currentRole
navigationPage responses currentRole (ExercisePage TenBandBoostCut) =
  runExerciseForNavigationPage tenBandBoostCutExercise responses currentRole
navigationPage responses currentRole (ExercisePage AddedWhiteNoise) =
  runExerciseForNavigationPage addedWhiteNoiseExercise responses currentRole
{- navigationPage responses currentRole (ExercisePage RT60) =
  runExerciseForNavigationPage rt60Exercise responses currentRole -}
navigationPage responses currentRole (ExercisePage Compression) =
  runExerciseForNavigationPage compressionExercise responses currentRole
navigationPage responses currentRole (ExercisePage OddEvenAll) =
  runExerciseForNavigationPage oddEvenAllExercise responses currentRole
navigationPage responses currentRole (ExercisePage SpectralShape) =
  runExerciseForNavigationPage spectralShapeExercise responses currentRole
navigationPage responses currentRole (ExercisePage Intervals1) =
  runExerciseForNavigationPage intervals1Exercise responses currentRole
navigationPage responses currentRole (ExercisePage FrequencyEnvelope) =
  runExerciseForNavigationPage frequencyEnvelopeExercise responses currentRole

{- navigationPage responses currentRole (ExercisePage LeftRightCentre) =
  runExerciseForNavigationPage leftRightCentreExercise responses currentRole -}

navigationPage responses currentRole TestPage = do
  testOurDynSvg
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses currentRole TestSoundPage = do
  (requests,sounds,navUnit) <- testSoundWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses currentRole AdminPage = do
  goToUserList <- (UserListPage <$) <$> button "Users"
  goToSplashPage <- (SplashPage <$) <$> button "Back to homepage"
  let navEvents = leftmost [goToUserList,goToSplashPage]
  let requests = never
  return (requests,never,navEvents)

navigationPage responses currentRole UserListPage = do
  (requests,nav) <- userListWidget responses currentRole
  let nav' = fmap (maybe AdminPage UserPage) nav
  return (requests,never,nav')

navigationPage responses currentRole (UserPage h) = do
  (requests,nav) <- userPageWidget h responses currentRole
  return (requests,never,SplashPage <$ nav)

runExerciseForNavigationPage :: (MonadWidget t m, Data c, Data q, Data a, Data e, Data s, Show c, Show q, Show a, Show e)
  => Exercise t m c q a e s
  -> Event t [Response] -> Dynamic t (Maybe Role)
  -> m (Event t Request,Event t Sound,Event t Navigation)
runExerciseForNavigationPage ex responses currentRole = do
  (newData,sounds,navUnit) <- runExercise ex responses
  currentRole' <- mapDyn isJust currentRole
  let newData' = gate (current currentRole') newData
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData'
  let newRequest = PostPoint <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

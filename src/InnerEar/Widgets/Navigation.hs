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
import Data.Map

import InnerEar.Types.Data
import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.CreateUser
import InnerEar.Widgets.Test
import Sound.MusicW
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
  TestPage

navigationWidget :: MonadWidget t m => Map String AudioBuffer -> Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request, Event t (Maybe (Synth ())))
navigationWidget sysResources responses currentRole = elClass "div" "mainBody" $ mdo
  let initialPage = navigationPage sysResources responses currentRole SplashPage
  let rebuild = fmap (navigationPage sysResources responses currentRole) navEvents
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

navigationPage :: MonadWidget t m => Map String AudioBuffer -> Event t [Response] -> Dynamic t (Maybe Role) -> Navigation -> m (Event t Request, Event t (Maybe (Synth ())), Event t Navigation)
navigationPage sysResources responses currentRole SplashPage = elClass "div" "nav" $ do
  elClass "div" "explanation" $
    text "Welcome to Inner Ear! Select an ear-training exercise from the list below. If you are doing this is part of a requirement for a class, please make sure you are logged in first (at the top right)."
  b <- mapM buttonForExercise includedExercises
  isAdmin <- mapDyn (==Just Administrator) currentRole
  a <- (AdminPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Administration")
  c <- (TestPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Test")
  d <- (CreateUserPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Create Users")
  let navEvents = leftmost (a:c:{-d:-}b)
  return (never,never,navEvents)

navigationPage sysResources responses currentRole CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage sysResources responses currentRole (ExercisePage ThresholdOfSilence) =
  runExerciseForNavigationPage sysResources thresholdOfSilenceExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage HarmonicDistortion) =
  runExerciseForNavigationPage sysResources harmonicDistortionExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage BoostOrCut) =
  runExerciseForNavigationPage sysResources boostOrCutExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage FiveBandBoostCut) =
  runExerciseForNavigationPage sysResources fiveBandBoostCutExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage TenBandBoostCut) =
  runExerciseForNavigationPage sysResources tenBandBoostCutExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage AddedWhiteNoise) =
  runExerciseForNavigationPage sysResources addedWhiteNoiseExercise responses currentRole
{- navigationPage sysResources responses currentRole (ExercisePage RT60) =
  runExerciseForNavigationPage sysResources rt60Exercise responses currentRole -}
navigationPage sysResources responses currentRole (ExercisePage Compression) =
  runExerciseForNavigationPage sysResources compressionExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage OddEvenAll) =
  runExerciseForNavigationPage sysResources oddEvenAllExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage SpectralShape) =
  runExerciseForNavigationPage sysResources spectralShapeExercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage Intervals1) =
  runExerciseForNavigationPage sysResources intervals1Exercise responses currentRole
navigationPage sysResources responses currentRole (ExercisePage FrequencyEnvelope) =
  runExerciseForNavigationPage sysResources frequencyEnvelopeExercise responses currentRole

{- navigationPage sysResources responses currentRole (ExercisePage LeftRightCentre) =
  runExerciseForNavigationPage sysResources leftRightCentreExercise responses currentRole -}

navigationPage sysResources responses currentRole TestPage = do
  -- testOurDynSvg
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage sysResources responses currentRole AdminPage = do
  goToUserList <- (UserListPage <$) <$> button "Users"
  goToSplashPage <- (SplashPage <$) <$> button "Back to homepage"
  let navEvents = leftmost [goToUserList,goToSplashPage]
  let requests = never
  return (requests,never,navEvents)

navigationPage sysResources responses currentRole UserListPage = do
  (requests,nav) <- userListWidget responses currentRole
  let nav' = fmap (maybe AdminPage UserPage) nav
  return (requests,never,nav')

navigationPage sysResources responses currentRole (UserPage h) = do
  (requests,nav) <- userPageWidget h responses currentRole
  return (requests,never,SplashPage <$ nav)

runExerciseForNavigationPage :: (MonadWidget t m, Data c, Data q, Data a, Data e, Data s, Show c, Show q, Show a, Show e)
  => Map String AudioBuffer
  -> Exercise t m c q a e s
  -> Event t [Response] -> Dynamic t (Maybe Role)
  -> m (Event t Request, Event t (Maybe (Synth ())), Event t Navigation)
runExerciseForNavigationPage sysResources ex responses currentRole = do
  (newData,sounds,navUnit) <- runExercise sysResources ex responses
  currentRole' <- mapDyn isJust currentRole
  let newData' = gate (current currentRole') newData
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData'
  let newRequest = PostPoint <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

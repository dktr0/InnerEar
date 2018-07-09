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

maintainExerciseStores :: MonadWidget t m => Event t [Response] -> Event t a -> m (Dynamic t (Map ExerciseId String))
maintainExerciseStores responses resetEvent = do
  let responseEvent = fmap ((Prelude.foldl (.) id) . fmap storeDBToMapChange . catMaybes . fmap justStoreResponses) responses
  let resetEvent' = (const Data.Map.empty) <$ resetEvent
  let updateEvents = mergeWith (.) [resetEvent',responseEvent]
  foldDyn ($) Data.Map.empty updateEvents

navigationWidget :: MonadWidget t m => Dynamic t (Map String AudioBuffer) -> Event t [Response] -> Dynamic t (Maybe Role) -> m (Event t Request, Event t (Maybe (Synth ())))
navigationWidget sysResources responses currentRole = elClass "div" "mainBody" $ mdo
  stores <- maintainExerciseStores responses $ updated currentRole
  let initialPage = navigationPage sysResources responses currentRole Data.Map.empty SplashPage
  let storesAndNav = attachDyn stores navEvents
  let rebuild = fmap (uncurry $ navigationPage sysResources responses currentRole) storesAndNav
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

navigationPage :: MonadWidget t m
  => Dynamic t (Map String AudioBuffer)
  -> Event t [Response]
  -> Dynamic t (Maybe Role)
  -> Map ExerciseId String
  -> Navigation
  -> m (Event t Request, Event t (Maybe (Synth ())), Event t Navigation)
navigationPage sysResources responses currentRole stores SplashPage = elClass "div" "nav" $ do
  elClass "div" "explanation" $
    text "Welcome to Inner Ear! Select an ear-training exercise from the list below. If you are doing this is part of a requirement for a class, please make sure you are logged in first (at the top right)."
  b <- mapM buttonForExercise includedExercises -- *** TODO: should use stores to display progress for each exercise
  isAdmin <- mapDyn (==Just Administrator) currentRole
  a <- (AdminPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Administration")
  c <- (TestPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Test")
  d <- (CreateUserPage <$) <$> (visibleWhen isAdmin $ elClass "div" "navButton" $ button "Create Users")
  let navEvents = leftmost (a:c:{-d:-}b)
  return (never,never,navEvents)

navigationPage sysResources responses currentRole stores CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage sysResources responses currentRole stores (ExercisePage ThresholdOfSilence) =
  runExerciseForNavigationPage sysResources thresholdOfSilenceExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage HarmonicDistortion) =
  runExerciseForNavigationPage sysResources harmonicDistortionExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage BoostOrCut) =
  runExerciseForNavigationPage sysResources boostOrCutExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage FiveBandBoostCut) =
  runExerciseForNavigationPage sysResources fiveBandBoostCutExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage TenBandBoostCut) =
  runExerciseForNavigationPage sysResources tenBandBoostCutExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage AddedWhiteNoise) =
  runExerciseForNavigationPage sysResources addedWhiteNoiseExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage Compression) =
  runExerciseForNavigationPage sysResources compressionExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage OddEvenAll) =
  runExerciseForNavigationPage sysResources oddEvenAllExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage SpectralShape) =
  runExerciseForNavigationPage sysResources spectralShapeExercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage Intervals1) =
  runExerciseForNavigationPage sysResources intervals1Exercise responses currentRole stores
navigationPage sysResources responses currentRole stores (ExercisePage FrequencyEnvelope) =
  runExerciseForNavigationPage sysResources frequencyEnvelopeExercise responses currentRole stores

navigationPage sysResources responses currentRole stores TestPage = do
  -- testOurDynSvg
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage sysResources responses currentRole stores AdminPage = do
  goToUserList <- (UserListPage <$) <$> button "Users"
  goToSplashPage <- (SplashPage <$) <$> button "Back to homepage"
  let navEvents = leftmost [goToUserList,goToSplashPage]
  let requests = never
  return (requests,never,navEvents)

navigationPage sysResources responses currentRole stores UserListPage = do
  (requests,nav) <- userListWidget responses currentRole
  let nav' = fmap (maybe AdminPage UserPage) nav
  return (requests,never,nav')

navigationPage sysResources responses currentRole stores (UserPage h) = do
  (requests,nav) <- userPageWidget h responses currentRole
  return (requests,never,SplashPage <$ nav)

runExerciseForNavigationPage :: (MonadWidget t m, Data c, Data q, Data a, Data e, Data s, JSON s, Show c, Show q, Show a, Show e, Show s)
  => Dynamic t (Map String AudioBuffer)
  -> Exercise t m c q a e s
  -> Event t [Response] -> Dynamic t (Maybe Role) -> Map ExerciseId String
  -> m (Event t Request, Event t (Maybe (Synth ())), Event t Navigation)
runExerciseForNavigationPage sysResources ex responses currentRole stores = do
  let initialStore = Data.Map.lookup (exerciseId ex) stores
  (newData,sounds,navUnit) <- runExercise initialStore sysResources ex responses
  currentRole' <- mapDyn isJust currentRole
  let newData' = gate (current currentRole') newData
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData'
  let newRequest = PostPoint <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

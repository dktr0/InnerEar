{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom
import Data.Time.Clock (getCurrentTime)

import InnerEar.Types.Data
import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.CreateUser
import InnerEar.Exercises.Prototype
import InnerEar.Widgets.Test
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise


data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request,Event t Sound)
navigationWidget responses = mdo
  let initialPage = navigationPage responses SplashPage
  let rebuild = fmap (navigationPage responses) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn (\(x,_,_) -> x) w
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_) -> x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x) -> x) w
  return (requests,sounds)

navigationPage :: MonadWidget t m => Event t [Response] -> Navigation -> m (Event t Request,Event t Sound,Event t Navigation)
navigationPage responses SplashPage = do
  w <- liftM (CreateUserPage <$) $ el "div" $ button "CreateUser"
  x <- liftM (ExercisePage <$)  $ el "div" $ button "Exercise"
  y <- liftM (TestPage <$)  $ el "div" $ button "Test"
  z <- liftM (TestSoundPage <$) $ el "div" $ button "Test Sound"
  let navEvents = leftmost [w,x,y,z]
  return (never,never,navEvents)

navigationPage responses CreateUserPage = do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage responses ExercisePage = do
  (newData,sounds,navUnit) <- runExercise prototypeExercise
  newPoint <- performEvent $ fmap (liftIO . datumToPoint . Left) $ newData
  let newRequest = PostRecord <$> Record "placeholderHandle" <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)

navigationPage responses TestPage = do
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses TestSoundPage = do
  (requests,sounds,navUnit) <- testSoundWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

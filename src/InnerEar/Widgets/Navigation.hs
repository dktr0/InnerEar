{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation where

import Control.Monad
import Reflex
import Reflex.Dom

import InnerEar.Types.Datum
import InnerEar.Widgets.CreateUser
import InnerEar.Exercises.Prototype
import InnerEar.Widgets.Test

data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Datum] -> m (Event t Datum)
navigationWidget responses = mdo
  let initialPage = navigationPage responses SplashPage
  let rebuild = fmap (navigationPage responses) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn fst w
  navEvents <- liftM switchPromptlyDyn $ mapDyn snd w
  return requests

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
  (newData,sounds,navUnit) <- createExerciseWidget prototypeExercise
  newPoint <- performIO $ fmap (liftIO . dataToPoint) $ newData
  let newRequest = PostRecord <$> Record "placeholderHandle" <$> newPoint
  return (newRequest,sounds,SplashPage <$ navUnit)
  where dataToPoint x = getCurrentTime >>= return . Point x
  
navigationPage responses TestPage = do
  (requests,sounds,navUnit) <- testWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

navigationPage responses TestSoundPage = do
  (requests,sounds,navUnit) <- testSoundWidget responses
  return (requests,sounds,SplashPage <$ navUnit)

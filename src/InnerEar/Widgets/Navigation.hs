{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation where

import Control.Monad
import Reflex
import Reflex.Dom

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Widgets.CreateUser
import InnerEar.Exercises.Prototype
import InnerEar.Widgets.Test

data Navigation =
  SplashPage |
  CreateUserPage |
  ExercisePage |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request)
navigationWidget responses = mdo
  let initialPage = navigationPage responses SplashPage
  let rebuild = fmap (navigationPage responses) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn fst w
  navEvents <- liftM switchPromptlyDyn $ mapDyn snd w
  return requests


navigationPage :: MonadWidget t m => Event t [Response] -> Navigation -> m (Event t Request,Event t Navigation)
navigationPage responses SplashPage = do
  w <- liftM (CreateUserPage <$) $ el "div" $ button "CreateUser"
  x <- liftM (ExercisePage <$)  $ el "div" $ button "Exercise"
  y <- liftM (TestPage <$)  $ el "div" $ button "Test"
  z <- liftM (TestSoundPage <$) $ el "div" $ button "Test Sound"
  let navEvents = leftmost [w,x,y,z]
  return (never,navEvents)

navigationPage responses CreateUserPage = createUserWidget responses >>= mapNavEventsToSplashPage
navigationPage responses ExercisePage = prototypeExercise responses >>= mapNavEventsToSplashPage
navigationPage responses TestPage = testWidget responses >>= mapNavEventsToSplashPage
navigationPage responses TestSoundPage = testSoundWidget responses >>= mapNavEventsToSplashPage



mapNavEventsToSplashPage :: MonadWidget t m => (a, Event t b) -> m (a, Event t Navigation)
mapNavEventsToSplashPage (x,y) = return $ (x,SplashPage <$ y)


{-
exerciseToPage :: MonadWidget t m
 => m (Event t Datum, Event t ())
 -> (Event t Response -> m (Event t Point,Event t()))
exerciseToPage w _ = do
  (data,navEvents) <- w
-}

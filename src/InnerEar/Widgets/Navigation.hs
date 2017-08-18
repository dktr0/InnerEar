{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module InnerEar.Widgets.Navigation (navigationWidget) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom
import Data.Time.Clock (getCurrentTime)

import InnerEar.Exercises.HarmonicsOne
import InnerEar.Types.Data
import InnerEar.Types.Response
import InnerEar.Types.Request
import InnerEar.Widgets.CreateUser
import InnerEar.Widgets.Sound
import InnerEar.Exercises.Prototype
import InnerEar.Widgets.Test
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.Exercise


data Navigation =
  SplashPage |
  CreateUserPage |
  TenBandsExercisePage |
  HarmonicsOneExercisePage |
  TestPage |
  TestSoundPage

navigationWidget :: MonadWidget t m => Event t [Response] -> m (Event t Request,Event t Sound)
navigationWidget responses = el "div" $ mdo
  let initialPage = navigationPage responses SplashPage
  let rebuild = fmap (navigationPage responses) navEvents
  w <- widgetHold initialPage rebuild
  requests <- liftM switchPromptlyDyn $ mapDyn (\(x,_,_) -> x) w
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_) -> x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x) -> x) w
  return (requests,sounds)

navigationPage :: MonadWidget t m => Event t [Response] -> Navigation -> m (Event t Request,Event t Sound,Event t Navigation)

navigationPage responses SplashPage = elClass "div" "nav" $ do
  a <- liftM (CreateUserPage <$) $ elClass "div" "navButton" $ button "CreateUser"
  b <- liftM (TenBandsExercisePage <$)  $ elClass "div" "navButton" $ button "Ten Bands Exercise"
  c <- liftM (HarmonicsOneExercisePage <$)  $ elClass "div" "navButton" $ button "Harmonics Exercise"
  d <- liftM (TestPage <$)  $ elClass "div" "navButton" $ button "Test"
  e <- liftM (TestSoundPage <$) $ elClass "div" "navButton" $ button "Test Sound"
  let navEvents = leftmost [a,b,c,d,e]
  return (never,never,navEvents)

navigationPage responses CreateUserPage = el "div" $ do
  (requests,navUnit) <- createUserWidget responses
  return (requests,never,SplashPage <$ navUnit)

navigationPage responses TenBandsExercisePage = runExerciseForNavigationPage prototypeExercise

navigationPage responses HarmonicsOneExercisePage = runExerciseForNavigationPage harmonicsOneExercise

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

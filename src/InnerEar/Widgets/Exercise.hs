{-# LANGUAGE RecursiveDo #-}

module InnerEar.Widgets.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Exercise


-- | runExercise takes a completely defined Exercise value and uses it to run an ear-training
-- exercise in the browser.

runExercise :: (MonadWidget t m, Show c, Show q, Show a, Show e) => Exercise t m c q a e -> m (Event t (ExerciseId,ExerciseDatum),Event t Sound,Event t ())
runExercise ex = mdo

  currentData <- foldDyn (:) [] newData -- ultimately this will include selected data from database as well
  nav <- holdDyn InConfigure navEvents

  -- Configure
  configVisible <- mapDyn (==InConfigure) nav
  configEvent <- visibleWhen configVisible $ elClass "div" "exerciseConfig" $ configWidget ex $ defaultConfig ex
  config <- holdDyn (defaultConfig ex) configEvent

  -- Question (with generateQuestion called again with each transition to Question)
  let triggerNewQuestion = ffilter (==InQuestion) navEvents
  configAndData <- combineDyn (,) config currentData -- Dynamic t (a,[Datum])
  let configAndData' = tagDyn configAndData triggerNewQuestion
  let questionIO = fmap (\(x,y) -> (generateQuestion ex) x y) configAndData'
  question <- performEvent $ fmap liftIO $ questionIO

  -- Question Widget
  let qWidget = fmap (\x-> (questionWidget ex) x (defaultEvaluation ex) question) (updated config)  -- Event t (m (Event,Event,Event)) 
  widgetEvents <- elClass "div" "exerciseQuestion" (widgetHold (return $ (never,never,never)) qWidget)  -- Dyn t (Ev, Ev, Ev)
  newData <- liftM switchPromptlyDyn $ mapDyn (\(a,_,_)->a) widgetEvents 
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,a,_)->a) widgetEvents
  questionNav <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a)->a) widgetEvents

  -- Display Evaluation
  displayEvalVisibile <- mapDyn (==InQuestion) nav
  let evalDataEv = fmapMaybe (\x-> case x of (Evaluation a)->Just a; otherwise->Nothing) newData
  evalData <- holdDyn (defaultEvaluation ex) evalDataEv
  displayEval <- visibleWhen displayEvalVisibile $ elClass "div" "displayEvaluation" $ do
    (displayEvaluation ex) evalData


  -- Reflect
  reflectVisible <- mapDyn (==InReflect) nav
  reflectNav <- visibleWhen reflectVisible $ elClass "div" "exerciseReflection" $ do
    text $ maybe "Uhoh - something went wrong" id (reflectiveQuestion ex)
    button "Submit Response"

  -- transitions between navigation modes
  let goToConfigure = ffilter (==InConfigure) questionNav
  let goToQuestion = leftmost [InQuestion <$ configEvent,ffilter (==InQuestion) questionNav]
  let maybeGoToReflect = ffilter (==InReflect) questionNav
  let goToReflect = fmapMaybe (\_ -> maybe Nothing (const $ Just InReflect) $ reflectiveQuestion ex) maybeGoToReflect
  let navEvents = leftmost [goToConfigure,goToQuestion,goToReflect]
  let closeExercise = fmapMaybe (\_ -> maybe (Just ()) (const Nothing) $ reflectiveQuestion ex ) maybeGoToReflect

  -- flattening and identification of exercise data for reporting/collection upwards
  let exerciseData = toExerciseDatum <$> newData
  let dataWithId = (\x -> (exerciseId ex,x)) <$> exerciseData
  return (dataWithId,sounds,closeExercise)

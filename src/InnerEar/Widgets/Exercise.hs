{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module InnerEar.Widgets.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Text.JSON
import Text.JSON.Generic

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Exercise


-- | runExercise takes a completely defined Exercise value and uses it to run an ear-training
-- exercise in the browser.

runExercise :: forall t m c q a e. (MonadWidget t m, Data c, Data q, Data a, Data e, Show c, Show q, Show a, Show e)
  => Exercise t m c q a e -> m (Event t (ExerciseId,ExerciseDatum),Event t Sound,Event t ())
runExercise ex = mdo

  currentData <- foldDyn (:) [] questionWidgetData -- ultimately this will include selected data from database as well

  -- Configure
  hackyBypass <- ((defaultConfig ex) <$) <$> getPostBuild
  config <- holdDyn (defaultConfig ex) $ leftmost [configUpdate,hackyBypass]

  -- Question Widget
  let qWidget = fmap (\x-> (questionWidget ex) x (defaultEvaluation ex) question) (updated config)  -- Event t (m (Event,Event,Event))
  widgetEvents <- elClass "div" "exerciseQuestion" (widgetHold (return $ (never,never,never,never)) qWidget)  -- Dyn t (Ev, Ev, Ev)
  sounds <- liftM switchPromptlyDyn $ mapDyn (\(_,a,_,_)->a) widgetEvents
  questionNav <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,a)->a) widgetEvents
  configUpdate <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a,_)->a) widgetEvents

  -- Question (with generateQuestion called again with each transition to Question)
  let triggerNewQuestion = leftmost [ffilter (==InQuestion) questionNav,InQuestion <$ hackyBypass]
  configAndData <- combineDyn (,) config currentData -- Dynamic t (a,[Datum])
  let configAndData' = tagDyn configAndData $ leftmost [triggerNewQuestion, InQuestion <$ configUpdate] -- also gen. new question on config update.
  let questionIO = fmap (\(x,y) -> (generateQuestion ex) x y) configAndData'
  question <- performEvent $ fmap liftIO $ questionIO

  let closeExercise = fmap (const ()) $ ffilter (==CloseExercise) questionNav

  -- structuring of exercise data for reporting/collection upwards
  startedData <- (Started <$) <$> getPostBuild
  let configData = Configured <$> configUpdate -- note: possiblity for data loss here with question event and leftmost
  let newQuestionData = attachDynWith (\c (q,a) -> NewQuestion c q a) config question
  questionWidgetData <- liftM switchPromptlyDyn $ mapDyn (\(a,_,_,_)->a) widgetEvents
  let endedData = Ended <$ closeExercise
  let allData = (leftmost [startedData,configData,newQuestionData,questionWidgetData,endedData]) :: Event t (Datum c q a e)
  let exerciseData = toExerciseDatum <$> allData
  let dataPairedWithId = (\x -> (exerciseId ex,x)) <$> exerciseData
  return (dataPairedWithId,sounds,closeExercise)

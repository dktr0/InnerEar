{-# LANGUAGE DeriveDataTypeable, RecursiveDo, ScopedTypeVariables #-}

module InnerEar.Widgets.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, void, join)
import Text.JSON
import Text.JSON.Generic
import Data.Maybe
import Data.Either
import Data.Map(Map)

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Exercise
import InnerEar.Types.Response

import Sound.MusicW

-- | runExercise takes a completely defined Exercise value and uses it to run an ear-training
-- exercise in the browser.

runExercise :: forall t m c q a e s. (MonadWidget t m, Data c, Data q, Data a, Data e, Data s, JSON s, Show c, Show q, Show a, Show e, Show s)
  => Maybe String
  -> Dynamic t (Map String AudioBuffer)
  -> Exercise t m c q a e s
  -> Event t [Response]
  -> m (Event t (ExerciseId,ExerciseDatum),Event t (Maybe (Synth ())),Event t ())
runExercise initialStore sysResources ex responses = mdo

  let initialStore' = fmap (maybe (defaultStore ex) id) $ join $ fmap storeValueToStore initialStore

  -- form databank for exercise by folding together pertinent database entries plus data transmitted up
  let records = ffilter (\x -> length x > 0) $ fmap (catMaybes . (fmap justRecordResponses)) responses -- Event t [Record]
  let points = fmap (fmap point) records -- Event t [Point], *** note: we probably shouldn't assume user handle matches...
  let datums = fmap (fmap datum) points
  let noSessionDatums = fmap lefts datums -- Event t [(ExerciseId,ExerciseDatum)]
  let datadown = fmap (fmap snd  . filter (\(i,_) -> i == exerciseId ex)) noSessionDatums -- Event t [ExerciseDatum]
  let questionWidgetData' = fmap (:[]) questionWidgetData
  currentData <- foldDyn (++) [] $ leftmost [datadown, questionWidgetData']

  -- Question Widget
  (questionWidgetData, sounds, configUpdate, questionNav) <- elClass "div" "exerciseQuestion" $ do
    (questionWidget ex) initialStore' sysResources (defaultConfig ex) (defaultEvaluation ex) question
  config <- holdDyn (defaultConfig ex) configUpdate

  -- Question (with generateQuestion called again with each transition to Question)
  hackyBypass <- getPostBuild
  let triggerNewQuestion = leftmost [ffilter (==InQuestion) questionNav,InQuestion <$ hackyBypass,InQuestion <$ configUpdate]
  configAndData <- combineDyn (,) config currentData -- Dynamic t (c,[Datum])
  let configAndData' = tagDyn configAndData triggerNewQuestion
  let questionIO = fmap (\(x,y) -> (generateQuestion ex) x y) configAndData'
  question <- performEvent $ fmap liftIO $ questionIO

  let closeExercise = fmap (const ()) $ ffilter (==CloseExercise) questionNav

  -- structuring of exercise data for reporting/collection upwards
  startedData <- (Started <$) <$> getPostBuild
  let configData = Configured <$> configUpdate -- note: possibility for data loss here with question event and leftmost
  let newQuestionData = attachDynWith (\c (q,a) -> NewQuestion c q a) config question

  let endedData = Ended <$ closeExercise
  let allData = (leftmost [startedData,configData,newQuestionData,endedData]) :: Event t (Datum c q a e s)
  let exerciseData = leftmost [questionWidgetData,toExerciseDatum <$> allData]
  let dataPairedWithId = (\x -> (exerciseId ex,x)) <$> exerciseData
  return (dataPairedWithId, sounds, closeExercise)

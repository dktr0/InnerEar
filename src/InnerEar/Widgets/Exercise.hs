{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module InnerEar.Widgets.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Text.JSON
import Text.JSON.Generic
import Data.Maybe
import Data.Either

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation
import InnerEar.Types.Exercise
import InnerEar.Types.Response

runExercise :: forall t m c q a e s. (MonadWidget t m, Data c, Data q, Data a, Data e, Show c, Show q, Show a, Show e) =>
  ExerciseId -> -- exerciseId
  ... -> -- questionWidget
  ... ->
  s ->
  m (Event t (ExerciseId,ExerciseDatum),Event t Sound,Event t ())

runExercise exerciseId store = mdo


  -- Question Widget
  (questionWidgetData,sounds,configUpdate,questionNav) <- elClass "div" "exerciseQuestion" $ do
    (questionWidget ex) (defaultConfig ex) (defaultEvaluation ex) question
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
  return (dataPairedWithId,sounds,closeExercise)

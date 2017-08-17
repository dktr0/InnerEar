{-# LANGUAGE RecursiveDo #-}

module InnerEar.Types.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Data
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation

-- | An Exercise is a uniquely typed value representing all of the components of
-- a functioning Inner Ear ear-training exercise. The types t and m are required by Reflex.
-- c represents the type of an exercise' configuration. q represents the type of a question.
-- a represents the type of an answer, and e represents the type of an evaluation (i.e. running score, etc).

data Exercise t m c q a e = Exercise {
  exerciseId :: ExerciseId,
  defaultConfig :: c,
  defaultEvaluation :: e,
  configWidget :: c -> m (Event t c), -- modal behaviour, i.e. issuing of config event also navigates to question
  generateQuestion :: c -> [Datum c q a e] -> IO (q,a),
  questionWidget :: e -> Event t (q,a) -> m (Event t (Datum c q a e),Event t Sound,Event t ExerciseNavigation),
  reflectiveQuestion :: Maybe Reflection -- where Nothing means no reflective question stage
}

-- | runExercise takes a completely defined Exercise value and uses it to run an ear-training
-- exercise in the browser.

runExercise :: (MonadWidget t m, Show c, Show q, Show a, Show e) => Exercise t m c q a e -> m (Event t (ExerciseId,ExerciseDatum),Event t Sound,Event t ())
runExercise ex = mdo

  currentData <- foldDyn (:) [] newData -- ultimately this will include selected data from database as well
  nav <- holdDyn InConfigure navEvents

  -- Configure
  configVisible <- mapDyn (==InConfigure) nav
  configEvent <- visibleWhen configVisible $ configWidget ex $ defaultConfig ex
  config <- holdDyn (defaultConfig ex) configEvent

  -- Question (with generateQuestion called again with each transition to Question)
  let triggerNewQuestion = ffilter (==InQuestion) navEvents
  configAndData <- combineDyn (,) config currentData -- Dynamic t (a,[Datum])
  let configAndData' = tagDyn configAndData triggerNewQuestion
  let questionIO = fmap (\(x,y) -> (generateQuestion ex) x y) configAndData'
  question <- performEvent $ fmap liftIO $ questionIO
  questionVisible <- mapDyn (==InQuestion) nav
  (newData,sounds,questionNav) <- visibleWhen questionVisible $ (questionWidget ex)  (defaultEvaluation ex) question
  
  --visibleWhen :: MonadWidget t m => Dynamic t Bool -> m a -> m a

  -- Reflect
  reflectVisible <- mapDyn (==InReflect) nav
  reflectNav <- visibleWhen reflectVisible $ do
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

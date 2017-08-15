{-# LANGUAGE RecursiveDo #-}

module InnerEar.Types.Exercise where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Datum
import Reflex.Synth.Synth
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation

data Exercise t m a b = Exercise { -- where a represents config type, b represents question type
  exerciseId :: ExerciseId,
  defaultConfig :: a,
  configWidget :: a -> m (Event t a), -- modal behaviour, i.e. issuing of config event also navigates to question
  generateQuestion :: a -> [Datum] -> IO b,
  questionWidget :: Event t b -> m (Event t Datum,Event t Sound,Event t ExerciseNavigation),
  reflectiveQuestion :: Maybe String -- where Nothing means no reflective question
}

createExerciseWidget :: MonadWidget t m => Exercise t m a b -> m (Event t Datum,Event t Sound,Event t ())
createExerciseWidget ex = mdo

  currentData <- foldDyn (:) [] newData -- ultimately this will include selected data from database as well
  nav <- holdDyn Configure navEvents

  -- Configure
  configVisible <- mapDyn (==Configure) nav
  configEvent <- visibleWhen configVisible $ configWidget ex $ defaultConfig ex
  config <- holdDyn (defaultConfig ex) configEvent

  -- Question (with generateQuestion called again with each transition to Question)
  let triggerNewQuestion = ffilter (==Question) navEvents
  configAndData <- combineDyn (,) config currentData -- Dynamic t (a,[Datum])
  let configAndData' = tagDyn configAndData triggerNewQuestion
  let questionIO = fmap (\(x,y) -> (generateQuestion ex) x y) configAndData'
  question <- performEvent $ fmap liftIO $ questionIO
  questionVisible <- mapDyn (==Question) nav
  (newData,sounds,questionNav) <- visibleWhen questionVisible $ questionWidget ex $ question

  -- Reflect
  reflectVisible <- mapDyn (==Reflect) nav
  reflectNav <- visibleWhen reflectVisible $ do
    text $ maybe "Uhoh - something went wrong" id (reflectiveQuestion ex)
    button "Submit Response"

  -- transitions between navigation modes
  let goToConfigure = ffilter (==Configure) questionNav
  let goToQuestion = leftmost [Question <$ configEvent,ffilter (==Question) questionNav]
  let maybeGoToReflect = ffilter (==Reflect) questionNav
  let goToReflect = fmapMaybe (\_ -> maybe Nothing (const $ Just Reflect) $ reflectiveQuestion ex) maybeGoToReflect
  let navEvents = leftmost [goToConfigure,goToQuestion,goToReflect]
  let closeExercise = fmapMaybe (\_ -> maybe (Just ()) (const Nothing) $ reflectiveQuestion ex ) maybeGoToReflect

  return (newData,sounds,closeExercise)

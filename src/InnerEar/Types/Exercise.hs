module InnerEar.Types.Exercise where

import Reflex
import Reflex.Dom

import InnerEar.Types.ExerciseId
import InnerEar.Widgets.Utility
import InnerEar.Types.Datum
import Reflex.Synth.Synth
import Reflex.Synth.Types

data Exercise a b = Exercise { -- where a represents config type, b represents question type
  exerciseId :: ExerciseId,
  defaultConfig :: a,
  configWidget :: MonadWidget t m => a -> m (Event t a), -- modal behaviour, i.e. issuing of config event also navigates to question
  generateQuestion :: a -> [Datum] -> IO b,
  questionWidget :: MonadWidget t m => Event t b -> m (Event t Datum,Event t Sound,Event t ExerciseNavigation),
  reflectiveQuestion :: Maybe String -- where Nothing means no reflective question
}

createExerciseWidget :: MonadWidget t m => Exercise a b -> m (Event t Datum,Event t Sound,Event t ())
createExerciseWidget ex rs = mdo

  data <- foldDyn (:) [] newData -- ultimately this will include selected data from database as well
  nav <- holdDyn Configure navEvents

  -- Configure
  configVisible <- mapDyn (==Configure) nav
  configEvent <- visibleWhen configVisible $ ex configWidget $ ex defaultConfig
  config <- holdDyn (ex defaultConfig) configEvent

  -- Question (with generateQuestion called again with each transition to Question)
  let triggerNewQuestion = ffilter (==Question) modeEvents
  let dataForNewQuestion = tagDyn data triggerNewQuestion
  questionIO <- combineDyn <$> config <*> dataForNewQuestion
  question <- performIO $ liftIO $ questionIO
  questionVisible <- mapDyn (==Question) nav
  (newData,sounds,questionNav) <- visibleWhen questionVisible $ ex questionWidget $ question

  -- Reflect
  reflectVisible <- mapDyn (==Reflect) nav
  reflectNav <- visibleWhen reflectVisible $ do
    text $ maybe "Uhoh - something went wrong" id (ex reflectiveQuestion)
    button "Submit Response"

  -- transitions between navigation modes
  let goToConfigure = ffilter (==Configure) questionNav
  let goToQuestion = leftmost [Question <$ config,ffilter (==Question) questionNav]
  let maybeGoToReflect = ffilter (==Reflect) questionNav
  let goToReflect = fmapMaybe (\_ -> maybe Nothing (const $ Just Reflect) $ ex reflectiveQuestion) maybeGoToReflect
  let navEvents = leftmost [goToConfigure,goToQuestion,goToReflect]
  let closeExercise = fmapMaybe (\_ -> maybe (Just ()) (const Nothing) $ ex reflectiveQuestion ) maybeGoToReflect

  return (newData,sounds,closeExercise)

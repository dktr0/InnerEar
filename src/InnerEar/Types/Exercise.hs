{-# LANGUAGE RecursiveDo #-}

module InnerEar.Types.Exercise where

import Reflex

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation


{- old model:
data Exercise t m c q a e s = Exercise {
  exerciseId :: ExerciseId,
  instructionsWidget :: m (),
  defaultConfig :: c,
  defaultEvaluation :: e,
  displayEvaluation :: Dynamic t e -> m (),
  generateQuestion :: c -> [ExerciseDatum] -> IO (q,a),
  questionWidget ::  c -> e -> Event t (q,a) -> m (Event t ExerciseDatum,Event t Sound, Event t c,Event t ExerciseNavigation)
}
-}

-- | An exercise is something that, given some initial store s (from the store map)
-- produces a widget that generates exercise data (including store updates and close events),
-- and sound events.

type Exercise t m s = s -> m (Event t (ExerciseId,ExerciseDatum),Event t Sound)

{-# LANGUAGE RecursiveDo #-}

module InnerEar.Types.Exercise where

import Reflex

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data
import Reflex.Synth.Types
import InnerEar.Types.ExerciseNavigation

-- | An Exercise is a uniquely typed value representing all of the components of
-- a functioning Inner Ear ear-training exercise. The types t and m are required by Reflex.
-- c represents the type of an exercise' configuration. q represents the type of a question.
-- a represents the type of an answer, and e represents the type of an evaluation (i.e. running score, etc).

data Exercise t m c q a e = Exercise {
  exerciseId :: ExerciseId,
  instructionsWidget :: m (),
  defaultConfig :: c,
  configWidget :: c -> m (Event t c),
  defaultEvaluation :: e,
  displayEvaluation :: Dynamic t e -> m (),
  generateQuestion :: c -> [Datum c q a e] -> IO (q,a),
  questionWidget ::  c -> e -> Event t (q,a) -> m (Event t (Datum c q a e),Event t Sound, Event t c,Event t ExerciseNavigation)
}

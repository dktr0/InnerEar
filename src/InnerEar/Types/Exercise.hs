{-# LANGUAGE RecursiveDo #-}

module InnerEar.Types.Exercise where

import Data.Map(Map)

import Reflex

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data
import Sound.MusicW
import InnerEar.Types.ExerciseNavigation

-- | An Exercise is a uniquely typed value representing all of the components of
-- a functioning Inner Ear ear-training exercise. The types t and m are required by Reflex.
-- c represents the type of an exercise' configuration. q represents the type of a question.
-- a represents the type of an answer, and e represents the type of an evaluation (i.e. running score, etc).

data Exercise t m c q a e s = Exercise {
  exerciseId :: ExerciseId,
  instructionsWidget :: m (),
  defaultConfig :: c,
  defaultEvaluation :: e,
  displayEvaluation :: Dynamic t e -> Dynamic t s -> m (),
  generateQuestion :: c -> [ExerciseDatum] -> IO (q,a),
  questionWidget :: Map String AudioBuffer -> c -> e -> Event t (q, a) -> m (Event t ExerciseDatum, Event t (Maybe (Synth ())), Event t c, Event t ExerciseNavigation)
}

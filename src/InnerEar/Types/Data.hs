{-# LANGUAGE DeriveDataTypeable #-}

-- | This module is a complete description of the data structure used in Inner Ear
-- to record user actions/choices, in order both to inform the adaptive generation
-- of new ear-training questions for that user, and to provide data for research analysis.

module InnerEar.Types.Data where

import Text.JSON
import Text.JSON.Generic
import Data.Tuple.Select
import Data.Tuple.Curry
import Data.Time.Clock
import Text.Read

import InnerEar.Types.Utility
import InnerEar.Types.ExerciseId
import InnerEar.Types.Handle

type Reflection = String


-- | Each exercise has unique strictly typed representations so there must be a type for the data
-- specific to each exercise. (And this type, Datum c q a e, like all of the types in this module, is
-- an automatically-derived instance of JSON, to facilitate communication back and forth with a server.)

data Datum c q a e = -- c is configuration type, q is question type, a is answer type, e is evaluation type
  Start |
  Configuration c |
  Question q a |
  Answer q a a | -- for convenience, an answer also includes the question and the correct answer
  Evaluation e |
  End (Maybe Reflection)
  deriving (Show,Eq,Data,Typeable)


-- | The functions in the definition of an exercise will return some specific type Datum c q a e
-- But to treat the data resulting from all exercises equally we need to be able to convert that
-- to and from a single data type, ExerciseDatum, using the functions toExerciseDatum and toDatum below.

data ExerciseDatum =
  ExerciseStart |
  ExerciseConfiguration String |
  ExerciseQuestion String |
  ExerciseAnswer String |
  ExerciseEvaluation String |
  ExerciseEnd (Maybe Reflection)
  deriving (Show,Eq,Data,Typeable)

toExerciseDatum :: (Show c,Show q,Show a,Show e) => Datum c q a e -> ExerciseDatum
toExerciseDatum Start = ExerciseStart
toExerciseDatum (Configuration c) = ExerciseConfiguration $ show c
toExerciseDatum (Question q correctAnswer) = ExerciseQuestion $ show (q,correctAnswer)
toExerciseDatum (Answer q correctAnswer userAnswer) = ExerciseAnswer $ show (q,correctAnswer,userAnswer)
toExerciseDatum (Evaluation e) = ExerciseEvaluation $ show e
toExerciseDatum (End r) = (ExerciseEnd r)

toDatum :: (Read c,Read q,Read a,Read e) => ExerciseDatum -> Maybe (Datum c q a e)
toDatum ExerciseStart = Just Start
toDatum (ExerciseConfiguration j) = Configuration <$> readMaybe j
toDatum (ExerciseQuestion j) = uncurry Question <$> readMaybe j
toDatum (ExerciseAnswer j) = uncurryN Answer <$> ((readMaybe j) :: (Read q, Read a) => Maybe (q,a,a))
toDatum (ExerciseEvaluation j) = Evaluation <$> readMaybe j
toDatum (ExerciseEnd r) = Just $ End r


-- | Some events of interest are not tied to a particular ear-training exercise.
-- For these, we have the type SessionDatum.

data SessionDatum = SessionStart | SessionEnd deriving (Show,Eq,Data,Typeable)


-- A Point of data, then, is either a tuple of (ExerciseId,ExerciseDatum) or SessionDatum
-- These points are what a running exercise widget (created using createExercise with a fully
-- defined Exercise) will both report upwards to the top level of the application, and use
-- in order to adaptively generate new questions.

type Time = UTCTime

data Point = Point {
  datum :: Either (ExerciseId,ExerciseDatum) SessionDatum,
  time :: Time
} deriving (Eq,Data,Typeable)

instance Show Point where
  show (Point d t) = show d ++ " (warning: not showing time)"

datumToPoint :: Either (ExerciseId,ExerciseDatum) SessionDatum -> IO Point
datumToPoint x = getCurrentTime >>= return . Point x


-- | The top level of our data structure is the Record, which is used for passing
-- data about an authenticated user back and forth between the server and the client.
-- When Inner Ear is used without a login/server, Records will play no role.

data Record = Record {
  userHandle :: Handle,
  point :: Point
} deriving (Show,Eq,Data,Typeable)

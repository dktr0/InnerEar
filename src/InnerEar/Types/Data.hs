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

-- | Each exercise has unique strictly typed representations so there must be a type for the data
-- specific to each exercise. (And this type, Datum c q a e, like all of the types in this module, is
-- an automatically-derived instance of JSON, to facilitate communication back and forth with a server.)

data Datum c q a e = -- c is configuration type, q is question type, a is answer type, e is evaluation type
  Started |
  Configured c |
  NewQuestion c q a |
  ListenedQuestion c q a |
  ListenedReference c q a |
  CorrectAnswer e e c q a | -- new short- and long-term evaluation plus context
  IncorrectAnswer a e e c q a | -- incorrect answer, new short- and long-term evaluation plus context
  ListenedExplore a c q a | -- for exploratory listening to possible answers
  Reflection String |
  Ended
  deriving (Show,Eq,Data,Typeable)


-- | The functions in the definition of an exercise will return some specific type Datum c q a e
-- But to treat the data resulting from all exercises equally we need to be able to convert that
-- to and from a single data type, ExerciseDatum, using the functions toExerciseDatum and toDatum below.

data ExerciseDatum =
  ExerciseStarted |
  ExerciseConfigured String |
  ExerciseNewQuestion String String String |
  ExerciseListenedQuestion String String String |
  ExerciseListenedReference String String String |
  ExerciseCorrectAnswer String String String String String |
  ExerciseIncorrectAnswer String String String String String String |
  ExerciseListenedExplore String String String String |
  ExerciseReflection String |
  ExerciseEnded
  deriving (Show,Eq,Data,Typeable)

toExerciseDatum :: (Data c,Data q,Data a,Data e) => Datum c q a e -> ExerciseDatum
toExerciseDatum Started = ExerciseStarted
toExerciseDatum (Configured c) = ExerciseConfigured $ encodeJSON c
toExerciseDatum (NewQuestion c q a) = ExerciseNewQuestion (encodeJSON c) (encodeJSON q) (encodeJSON a)
toExerciseDatum (ListenedQuestion c q a) = ExerciseListenedQuestion (encodeJSON c) (encodeJSON q) (encodeJSON a)
toExerciseDatum (ListenedReference c q a) = ExerciseListenedReference (encodeJSON c) (encodeJSON q) (encodeJSON a)
toExerciseDatum (CorrectAnswer e1 e2 c q a) = ExerciseCorrectAnswer (encodeJSON e1) (encodeJSON e2) (encodeJSON c) (encodeJSON q) (encodeJSON a)
toExerciseDatum (IncorrectAnswer ia e1 e2 c q a) = ExerciseCorrectAnswer (encodeJSON ia) (encodeJSON e1) (encodeJSON e2) (encodeJSON c) (encodeJSON q) (encodeJSON a)
toExerciseDatum (ListenedExplore a1 c q a2) = ExerciseListenedExplore (encodeJSON a1) (encodeJSON c) (encodeJSON q) (encodeJSON a2)
toExerciseDatum (Reflection r) = ExerciseReflection $ encodeJSON r
toExerciseDatum Ended = ExerciseEnded

toDatum :: (JSON c, JSON q, JSON a, JSON e) => ExerciseDatum -> Result (Datum c q a e)
toDatum ExerciseStarted = return Started
toDatum (ExerciseConfigured j) = Configured <$> fromJSON j
toDatum (ExerciseNewQuestion c q a) = NewQuestion <$> fromJSON c <*> fromJSON q <*> fromJSON a
toDatum (ExerciseListenedQuestion c q a) = ListenedQuestion <$> fromJSON c <*> fromJSON q <*> fromJSON a
toDatum (ExerciseListenedReference c q a) = ListenedReference <$> fromJSON c <*> fromJSON q <*> fromJSON a
toDatum (ExerciseCorrectAnswer e1 e2 c q a) = CorrectAnswer <$> fromJSON e1 <*> fromJSON e2 <*> fromJSON c <*> fromJSON q <*> fromJSON a
toDatum (ExerciseIncorrectAnswer ia e1 e2 c q a) = CorrectAnswer <$> fromJSON ia <*> fromJSON e1 <*> fromJSON e2 <*> fromJSON c <*> fromJSON q <*> fromJSON a
toDatum (ExerciseListenedExplore a1 c q a2) = ListenedReference <$> fromJSON a1 <*> fromJSON c <*> fromJSON q <*> fromJSON a2
toDatum (ExerciseReflection r) = return $ Reflection r
toDatum ExerciseEnd = return Ended


-- | Some events of interest are not tied to a particular ear-training exercise.
-- For these, we have the type SessionDatum.

data SessionDatum = SessionStart | SessionEnd | AuthenticationFailure deriving (Show,Eq,Data,Typeable)


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

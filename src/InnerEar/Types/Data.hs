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
import Data.Map

import InnerEar.Types.Utility
import InnerEar.Types.ExerciseId
import InnerEar.Types.Handle

-- | Each exercise has unique strictly typed representations so there must be a type for the data
-- specific to each exercise. (And this type, Datum c q a e, like all of the types in this module, is
-- an automatically-derived instance of JSON, to facilitate communication back and forth with a server.)

data Datum c q a e s = -- c is configuration type, q is question type, a is answer type, e is evaluation type, s is exercise store type
  Started |
  Configured c |
  NewQuestion q a |
  ListenedQuestion |
  ListenedReference |
  IncorrectAnswer a s |
  CorrectAnswer s |
  ListenedExplore a |
  Reflection String |
  Ended
  deriving (Show,Eq,Data,Typeable)

instance (Data c, Data q, Data a, Data e, Data s) => JSON (Datum c q a e s) where
  showJSON = toJSON
  readJSON = fromJSON

-- | The functions in the definition of an exercise will return some specific type Datum c q a e
-- But to treat the data resulting from all exercises equally we need to be able to convert that
-- to and from a single data type, ExerciseDatum, using the functions toExerciseDatum and toDatum below.

data ExerciseDatum =
  ExerciseStarted |
  ExerciseConfigured String |
  ExerciseNewQuestion String String |
  ExerciseListenedQuestion |
  ExerciseListenedReference |
  ExerciseIncorrectAnswer String String |
  ExerciseCorrectAnswer String |
  ExerciseListenedExplore String |
  ExerciseReflection String |
  ExerciseEnded
  deriving (Show,Eq,Data,Typeable)

toExerciseDatum :: (Data c,Data q,Data a,Data e,Data s) => Datum c q a e s -> ExerciseDatum
toExerciseDatum Started = ExerciseStarted
toExerciseDatum (Configured c) = ExerciseConfigured $ encodeJSON c
toExerciseDatum (NewQuestion q a) = ExerciseNewQuestion (encodeJSON q) (encodeJSON a)
toExerciseDatum ListenedQuestion = ExerciseListenedQuestion
toExerciseDatum ListenedReference = ExerciseListenedReference
toExerciseDatum (IncorrectAnswer a s) = ExerciseIncorrectAnswer (encodeJSON a) (encodeJSON s)
toExerciseDatum (CorrectAnswer s) = ExerciseCorrectAnswer (encodeJSON s)
toExerciseDatum (ListenedExplore a) = ExerciseListenedExplore (encodeJSON a)
toExerciseDatum (Reflection r) = ExerciseReflection r
toExerciseDatum Ended = ExerciseEnded


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


data StoreDB = StoreDB {
  storeHandle :: Handle,
  storeId :: ExerciseId,
  storeValue :: String, -- JSON representation of exercise store
  storeTime :: Time
  } deriving (Eq,Data,Typeable)

instance JSON StoreDB where
  showJSON = toJSON
  readJSON = fromJSON

instance Show StoreDB where
  show x = show (storeHandle x) ++ " " ++ show (storeId x) ++ " " ++ show (storeValue x)

storeDBToMapChange :: StoreDB -> Map ExerciseId String -> Map ExerciseId String
storeDBToMapChange x = insert (storeId x) (storeValue x)

type StoreString = String

storeStringToStore :: JSON s => StoreString -> Maybe s
storeStringToStore = f . decode
  where f (Ok x) = Just x
        f _ = Nothing

storeToStoreString :: JSON s => s -> StoreString
storeToStoreString = encode

recordToMaybeStoreDB :: Record -> Maybe StoreDB
recordToMaybeStoreDB (Record h (Point (Left (i,ExerciseIncorrectAnswer _ s)) t)) = Just (StoreDB h i s t)
recordToMaybeStoreDB (Record h (Point (Left (i,ExerciseCorrectAnswer s)) t)) = Just (StoreDB h i s t)
recordToMaybeStoreDB _ = Nothing

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module InnerEar.Database.Events where

import Text.Read
import Data.Either
import Data.Maybe
import Data.Text
import Data.Char
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import InnerEar.Types.ExerciseId
import InnerEar.Types.Handle
import InnerEar.Types.Data
import InnerEar.Types.ExerciseId
import InnerEar.Database.ExerciseId

-- definitions pertaining to "events", ie. the type Record and all of its dependencies

createEventsTable :: Connection -> IO ()
createEventsTable c =
  execute_ c "CREATE TABLE IF NOT EXISTS events (handle TEXT, time TEXT, event TEXT, exerciseId TEXT, param1 TEXT, param2 TEXT)"

instance FromRow Record where
  fromRow = do
    (h,t,e,i,p1,p2) <- fromRow
    let x = recordFromRow h t e i p1 p2
    let uhoh = fail "unable to parse Record in FromRow instance"
    maybe uhoh return x

recordFromRow :: String -> Time -> String -> ExerciseId -> String -> String -> Maybe Record
recordFromRow h t "Started" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseStarted)) t
recordFromRow h t "Configured" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseConfigured p1)) t
recordFromRow h t "NewQuestion" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseNewQuestion p1 p2)) t
recordFromRow h t "ListenedQuestion" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseListenedQuestion)) t
recordFromRow h t "ListenedReference" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseListenedReference)) t
recordFromRow h t "IncorrectAnswer" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseIncorrectAnswer p1 p2)) t
recordFromRow h t "CorrectAnswer" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseCorrectAnswer p1)) t
recordFromRow h t "ListenedExplore" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseListenedExplore p1)) t
recordFromRow h t "Reflection" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseReflection p1)) t
recordFromRow h t "Ended" i p1 p2 = Just $ Record h $ Point (Left (i,ExerciseEnded)) t
recordFromRow h t "SessionStart" i p1 p2 = Just $ Record h $ Point (Right SessionStart) t
recordFromRow h t "SessionEnd" i p1 p2 = Just $ Record h $ Point (Right SessionEnd) t
recordFromRow h t "AuthenticationFailure" i p1 p2 = Just $ Record h $ Point (Right AuthenticationFailure) t
recordFromRow _ _ _ _ _ _ = Nothing

instance ToRow Record where
  toRow (Record h (Point (Left (i,ExerciseStarted)) t)) = toRow (h,t,"Started"::Text,i,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseConfigured c)) t)) = toRow (h,t,"Configured"::Text,i,c,nil)
  toRow (Record h (Point (Left (i,ExerciseNewQuestion q a)) t)) = toRow (h,t,"NewQuestion"::Text,i,q,a)
  toRow (Record h (Point (Left (i,ExerciseListenedQuestion)) t)) = toRow (h,t,"ListenedQuestion"::Text,i,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseListenedReference)) t)) = toRow (h,t,"ListenedReference"::Text,i,nil,nil)
  toRow (Record h (Point (Left (i,ExerciseIncorrectAnswer a s)) t)) = toRow (h,t,"IncorrectAnswer"::Text,i,a,s)
  toRow (Record h (Point (Left (i,ExerciseCorrectAnswer s)) t)) = toRow (h,t,"CorrectAnswer"::Text,i,s,nil)
  toRow (Record h (Point (Left (i,ExerciseListenedExplore a)) t)) = toRow (h,t,"ListenedExplore"::Text,i,a,nil)
  toRow (Record h (Point (Left (i,ExerciseReflection r)) t)) = toRow (h,t,"Reflection"::Text,i,r,nil)
  toRow (Record h (Point (Left (i,ExerciseEnded)) t)) = toRow (h,t,"Ended"::Text,i,nil,nil)
  toRow (Record h (Point (Right SessionStart) t)) = toRow (h,t,"SessionStart"::Text,nil,nil,nil)
  toRow (Record h (Point (Right SessionEnd) t)) = toRow (h,t,"SessionEnd"::Text,nil,nil,nil)
  toRow (Record h (Point (Right AuthenticationFailure) t)) = toRow (h,t,"AuthenticationFailure"::Text,nil,nil,nil)

nil :: SQLData
nil = toField (Nothing :: Maybe Text)

postEvent :: Connection -> Record -> IO ()
postEvent c r = execute c "INSERT INTO events (handle,time,event,exerciseId,param1,param2) VALUES (?,?,?,?,?,?)" $ sanitizeRecord r

sanitizeRecord :: Record -> Record
sanitizeRecord r = r { userHandle = fmap Data.Char.toLower (userHandle r) }

findAllRecords :: Connection -> Handle -> IO [Record]
findAllRecords conn h = query conn "SELECT handle,time,event,exerciseId,param1,param2 FROM events WHERE handle = ?" (Only (fmap Data.Char.toLower h))

findAllExerciseEvents :: Connection -> Handle -> ExerciseId -> IO [Record]
findAllExerciseEvents conn h e = query conn "SELECT handle,time,event,exerciseId,param1,param2 FROM events WHERE handle = ? AND exerciseId = ?" (fmap Data.Char.toLower h,show e)

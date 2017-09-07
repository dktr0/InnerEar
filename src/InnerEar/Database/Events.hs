{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Database.Events where

import Data.Either
import Data.Maybe
import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import InnerEar.Types.ExerciseId
import InnerEar.Types.Data

-- definitions pertaining to "events", ie. the type Record and all of its dependencies

createEventsTable :: Connection -> IO ()
createEventsTable c =
  execute_ c "CREATE TABLE IF NOT EXISTS events (handle TEXT, time TEXT, event TEXT, id TEXT, content TEXT)"

instance FromField ExerciseId where
  fromField = f . read . g . fieldData
    where g (SQLText t) = unpack t
          g _ = ""
          f (Just x) = Ok x
    -- *** incomplete pattern matching here is a bad bad bad temporary idea...

instance ToField ExerciseId where
  toField = SQLText . pack . show

instance FromRow Record where
  fromRow = do
    (h,t,e,i,c) <- fromRow
    let r = Record <$> h <*> pointFromPartialRow e i c t
    let uhoh = fail "unable to parse Record in FromRow instance"
    maybe uhoh return r

pointFromPartialRow :: Maybe String -> Maybe ExerciseId -> Maybe String -> Maybe Time -> Maybe Point
pointFromPartialRow (Just "ExerciseStart") (Just i) _ (Just t) = Just (Point (Left (i,ExerciseStart)) t)
pointFromPartialRow (Just "ExerciseConfiguration") (Just i) (Just c) (Just t) = Just (Point (Left (i,ExerciseConfiguration c)) t)
pointFromPartialRow (Just "ExerciseQuestion") (Just i) (Just c) (Just t) = Just (Point (Left (i,ExerciseQuestion c)) t)
pointFromPartialRow (Just "ExerciseEvaluation") (Just i) (Just c) (Just t) = Just (Point (Left (i,ExerciseEvaluation c)) t)
pointFromPartialRow (Just "ExerciseReflection") (Just i) (Just c) (Just t) = Just (Point (Left (i,ExerciseReflection c)) t)
pointFromPartialRow (Just "ExerciseEnd") (Just i) _ (Just t) = Just (Point (Left (i,ExerciseEnd)) t)
pointFromPartialRow (Just "SessionStart") _ _ (Just t) = Just (Point (Right SessionStart) t)
pointFromPartialRow (Just "SessionEnd") _ _ (Just t) = Just (Point (Right SessionEnd) t)
pointFromPartialRow _ _ _ _ = Nothing

instance ToRow Record where
  toRow (Record h (Point (Left (i,ExerciseStart)) t)) = toRow (h,t,"ExerciseStart"::Text,i,Nothing :: Maybe Text)
  toRow (Record h (Point (Left (i,ExerciseConfiguration s)) t)) = toRow (h,t,"ExerciseConfiguration"::Text,i,s)
  toRow (Record h (Point (Left (i,ExerciseQuestion s)) t)) = toRow (h,t,"ExerciseQuestion"::Text,i,s)
  toRow (Record h (Point (Left (i,ExerciseAnswer s)) t)) = toRow (h,t,"ExerciseAnswer"::Text,i,s)
  toRow (Record h (Point (Left (i,ExerciseEvaluation s)) t)) = toRow (h,t,"ExerciseEvaluation"::Text,i,s)
  toRow (Record h (Point (Left (i,ExerciseReflection s)) t)) = toRow (h,t,"ExerciseReflection"::Text,i,s)
  toRow (Record h (Point (Left (i,ExerciseEnd)) t)) = toRow (h,t,"ExerciseEnd"::Text,i,Nothing :: Maybe Text)
  toRow (Record h (Point (Right SessionStart) t)) = toRow (h,t,"SessionStart"::Text,Nothing :: Maybe Text,Nothing :: Maybe Text)
  toRow (Record h (Point (Right SessionEnd) t)) = toRow (h,t,"SessionEnd"::Text,Nothing :: Maybe Text,Nothing :: Maybe Text)


postEvent :: Connection -> Record -> IO ()
postEvent c r = execute c "INSERT INTO events (handle,time,event,id,content) VALUES (?,?,?,?,?)" r

-- don't use this with a real database...
findAllEvents :: Connection -> IO [Record]
findAllEvents c = query_ c "SELECT handle,time,event,id,content FROM events"


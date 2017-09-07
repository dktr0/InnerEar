{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Database.SQLite where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import Data.Time.Clock

import InnerEar.Types.User
import InnerEar.Types.Data
import InnerEar.Types.ExerciseId
import InnerEar.Database.Users
import InnerEar.Database.Events

openDatabase :: IO Connection
openDatabase = do
  c <- open "../InnerEar.db"
  createUsersTable c
  createEventsTable c
  return c

closeDatabase :: Connection -> IO ()
closeDatabase = close

databaseTest :: IO ()
databaseTest = do
  now <- getCurrentTime
  db <- openDatabase
  addUser db $ User "d0kt0r0" "test" Administrator
  postEvent db $ Record "d0kt0r0" (Point (Left (ThresholdOfSilence,ExerciseStart)) now)
  print "Users:"
  findAllUsers db >>= print
  print "Events:"
  findAllEvents db >>= print
  closeDatabase db

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
  addUser c $ User "test" "test" NormalUser
  return c

closeDatabase :: Connection -> IO ()
closeDatabase = close

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module InnerEar.Database.ExerciseId where

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

instance FromField ExerciseId where
  fromField = f . readMaybe . g . fieldData
    where g (SQLText t) = unpack t
          g _ = ""
          f (Just x) = Ok x
    -- *** incomplete pattern matching here is a bad bad bad temporary idea...

instance ToField ExerciseId where
  toField = SQLText . pack . show


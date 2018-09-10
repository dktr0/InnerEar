{-# LANGUAGE OverloadedStrings, TupleSections #-}

module InnerEar.Database.Stores where

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

createStoresTable :: Connection -> IO ()
createStoresTable c = execute_ c "CREATE TABLE IF NOT EXISTS stores (handle TEXT NOT NULL, exerciseId TEXT NOT NULL, store TEXT, time TEXT, PRIMARY KEY (handle,exerciseId))"

instance FromRow StoreDB where
  fromRow = StoreDB <$> field <*> field <*> field <*> field

instance ToRow StoreDB where
  toRow (StoreDB h i s t) = toRow (h,i,s,t)

postStore :: Connection -> StoreDB -> IO ()
postStore c s = execute c "INSERT OR REPLACE INTO stores VALUES (?,?,?,?)" s

findAllStores :: Connection -> Handle -> IO [StoreDB]
findAllStores conn h = query conn "SELECT handle,exerciseId,store,time FROM stores WHERE handle = ?" (Only (fmap Data.Char.toLower h))

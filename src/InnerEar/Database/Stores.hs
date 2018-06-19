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
createStoresTable c = execute_ c "CREATE TABLE IF NOT EXISTS stores (handle TEXT, exerciseId TEXT, store TEXT, time TEXT)"

instance FromRow StoreDB where
  fromRow = StoreDB <$> field <*> field <*> field <*> field 

instance ToRow StoreDB where
  toRow (StoreDB h i s t) = toRow (h,i,s,t)

-- TODO: this is not quite right: there should be at most one entry in the database for each handle and exerciseId combination
-- so if there is already an entry for this handle+exerciseID, it must be replaced with the new store and time
postStore :: Connection -> StoreDB -> IO ()
postStore c s = execute c "INSERT INTO stores (handle,exerciseId,store,time)" s

findAllStores :: Connection -> Handle -> IO [StoreDB]
findAllStores conn h = query conn "SELECT handle,exerciseId,store,time FROM stores WHERE handle = ?" (Only (fmap Data.Char.toLower h))


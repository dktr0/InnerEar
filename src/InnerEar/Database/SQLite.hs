{-# LANGUAGE OverloadedStrings #-}

module InnerEar.Database.SQLite where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.User

establishDatabase :: IO Connection
establishDatabase = do
  c <- open "InnerEar.db"
  establishUsersTable c
  return c


establishUsersTable :: Connection -> IO ()
establishUsersTable c = execute_ c "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, handle TEXT, password TEXT, canModifyUsers BIT)"

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User h pwd cmu) = toRow (h,pwd,cmu)

addUser :: Connection -> User -> IO ()
addUser c u = execute c "INSERT INTO users (handle,password,canModifyUsers) VALUES (?,?,?)" u

findUser :: Connection -> Handle -> IO (Maybe User)
findUser conn h = do
  r <- query conn "SELECT (handle,password,canModifyUsers) FROM users WHERE handle = ?" (pack h)
  return $ f r
  where
    f [] = Nothing
    f (x:_) = Just x

findAllUsers :: Connection -> IO [User]
findAllUsers c = query_ c "SELECT (handle,password,canModifyUsers) FROM USERS"

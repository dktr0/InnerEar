{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Response where

import Text.JSON
import Text.JSON.Generic
import Text.JSON.String (runGetJSON)

import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Data
import InnerEar.Types.User
import InnerEar.Types.ExerciseId

data Response =
  NotAuthenticated | -- signals that client is not authenticated as any handle
  Authenticated Handle Role | -- signals that client is successfully authenticated as the indicated handle and role
  UserNotCreated String | -- signals a failure to create a new user for some reason (described by the String)
  UserCreated | -- signals success at creating a new normal user
  RecordResponse Record |
  StoreResponse StoreDB |
  AllStoresSent |
  UserData User
  deriving (Show,Eq,Data,Typeable)

instance JSON Response where
  showJSON = toJSON
  readJSON = fromJSON

-- | decodeResponse probably shouldn't be necessary but we need to do this
-- in order to avoid "unknown constructor" errors when decoding JSON
-- *** note: maybe it is no longer necessary since we added JSON instance just above?

decodeResponses :: String -> Result [Response]
decodeResponses = either Error fromJSON . runGetJSON readJSValue

getHandleFromAuthenticated :: Response -> Maybe Handle
getHandleFromAuthenticated (Authenticated h _) = Just h
getHandleFromAuthenticated _ = Nothing

isAuthenticated :: Response -> Bool
isAuthenticated (Authenticated _ _) = True
isAuthenticated _ = False

justRecordResponses :: Response -> Maybe Record
justRecordResponses (RecordResponse r) = Just r
justRecordResponses _ = Nothing

justStoreResponses :: Response -> Maybe StoreDB
justStoreResponses (StoreResponse s) = Just s
justStoreResponses _ = Nothing

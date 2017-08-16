{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Response where

import Text.JSON
import Text.JSON.Generic
import Text.JSON.String (runGetJSON)

import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Data

data Response =
  NotAuthenticated | -- signals that client is not authenticated as any handle
  Authenticated Handle | -- signals that client is successfully authenticated as the indicated handle
  UserNotCreated | -- signals a failure to create a new user for some reason
  Downloaded Record
  deriving (Show,Eq,Data,Typeable)


-- | decodeResponse probably shouldn't be necessary but we need to do this
-- in order to avoid "unknown constructor" errors when decoding JSON

decodeResponses :: String -> Result [Response]
decodeResponses = either Error fromJSON . runGetJSON readJSValue

getHandleFromAuthenticated :: Response -> Maybe Handle
getHandleFromAuthenticated (Authenticated h) = Just h
getHandleFromAuthenticated _ = Nothing

isAuthenticated :: Response -> Bool
isAuthenticated (Authenticated _) = True
isAuthenticated _ = False

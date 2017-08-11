module InnerEar.Types.Response where

import Text.JSON
import Data.List (find)

import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Record

data Response =
  NotAuthenticated | -- signals that client is not authenticated as any handle
  Authenticated Handle | -- signals that client is successfully authenticated as the indicated handle
  UserNotCreated | -- signals a failure to create a new user for some reason
  Downloaded Record
  deriving (Show,Eq)

instance JSON Response where
  showJSON (NotAuthenticated) = showJSON "NotAuthenticated"
  showJSON (Authenticated h) = encJSDict [("Authenticated",h)]
  showJSON (UserNotCreated) = showJSON "UserNotCreated"
  showJSON (Downloaded r) = encJSDict [("Downloaded",r)]
  readJSON (JSString x) | fromJSString x == "NotAuthenticated" = Ok NotAuthenticated
  readJSON (JSObject x) | firstKey x == "Authenticated" = Authenticated <$> valFromObj "Authenticated" x
  readJSON (JSString x) | fromJSString x == "UserNotCreated" = Ok UserNotCreated
  readJSON (JSObject x) | firstKey x == "Downloaded" = Downloaded <$> valFromObj "Downloaded" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Response: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Response: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Response"

getHandleFromAuthenticated :: Response -> Maybe Handle
getHandleFromAuthenticated (Authenticated h) = Just h
getHandleFromAuthenticated _ = Nothing

isAuthenticated :: Response -> Bool
isAuthenticated (Authenticated _) = True
isAuthenticated _ = False

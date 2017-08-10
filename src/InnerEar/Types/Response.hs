module InnerEar.Types.Response where

import Text.JSON
import Data.List (find)

import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Record

data Response =
  NotAuthenticated | -- signals that client is not authenticated as any handle
  Authenticated Handle | -- signals that client is successfully authenticated as the indicated handle
  Downloaded Record
  deriving (Show,Eq)

instance JSON Response where
  showJSON (NotAuthenticated) = showJSON "NotAuthenticated"
  showJSON (Authenticated h) = encJSDict [("Authenticated",h)]
  showJSON (Downloaded r) = encJSDict [("Downloaded",r)]
  readJSON (JSString x) | fromJSString x == "NotAuthenticated" = Ok NotAuthenticated
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Response: " ++ (show x)
  readJSON (JSObject x) | firstKey x == "Authenticated" = Authenticated <$> valFromObj "Authenticated" x
  readJSON (JSObject x) | firstKey x == "Downloaded" = Downloaded <$> valFromObj "Downloaded" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Response: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Response"

getHandleFromAuthenticated :: Response -> Maybe Handle
getHandleFromAuthenticated (Authenticated h) = Just h
getHandleFromAuthenticated _ = Nothing

lastAuthenticationRelatedResponse :: [Response] -> Maybe Response
lastAuthenticationRelatedResponse = find f . reverse
  where f (Authenticated x) = True
        f (NotAuthenticated) = True
        f _ = False

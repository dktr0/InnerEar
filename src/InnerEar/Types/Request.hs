{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Request where

import Text.JSON
import Text.JSON.Generic
import Text.JSON.String (runGetJSON)
import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Record

data Request =
  CreateUser Handle Password |
  Authenticate Handle Password |
  Deauthenticate |
  PostRecord Record
  deriving (Eq,Show,Data,Typeable)


-- | decodeRequest probably shouldn't be necessary but we need to do this
-- in order to avoid "unknown constructor" errors when decoding JSON requests
-- on the server side, for no obvious reason. A small price to pay for auto-derived
-- JSON instances though...

decodeRequest :: String -> Result Request
decodeRequest = either Error fromJSON . runGetJSON readJSValue

{-
instance JSON Request where
  showJSON (CreateUser h p) = encJSDict [("CreateUser",h),("p",p)]
  showJSON (Authenticate h p) = encJSDict [("Authenticate",h),("p",p)]
  showJSON (Deauthenticate) = showJSON "Deauthenticate"
  showJSON (PostRecord p) = encJSDict [("PostRecord",p)]
  readJSON (JSObject x) | firstKey x == "CreateUser" = CreateUser <$> valFromObj "CreateUser" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x <*> valFromObj "p" x
  readJSON (JSString x) | fromJSString x == "Deauthenticate" = Ok Deauthenticate
  readJSON (JSObject x) | firstKey x == "PostRecord" = PostRecord <$> valFromObj "PostRecord" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Request: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject or JSString as Request"
-}

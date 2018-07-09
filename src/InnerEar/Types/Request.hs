{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Request where

import Text.JSON
import Text.JSON.Generic
import Text.JSON.String (runGetJSON)
import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Data
import InnerEar.Types.ExerciseId

data Request =
  CreateUser Handle Password |
  Authenticate Handle Password |
  Deauthenticate |
  PostPoint Point |
  GetUserList |
  GetAllRecords Handle |
  GetAllExerciseEvents Handle ExerciseId
  deriving (Eq,Show,Data,Typeable)

instance JSON Request where
  showJSON = toJSON
  readJSON = fromJSON

-- | decodeRequest probably shouldn't be necessary but we need to do this
-- in order to avoid "unknown constructor" errors when decoding JSON requests
-- for no obvious reason. A small price to pay for auto-derived JSON instances though...
-- *** note: maybe it is no longer necessary with JSON instance above though??

decodeRequest :: String -> Result Request
decodeRequest = either Error fromJSON . runGetJSON readJSValue

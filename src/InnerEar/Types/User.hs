{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.User where

import Text.JSON
import Text.JSON.Generic


import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Data

data Role =
  NormalUser | -- can only log in, do exercises, inspect their own data/history
  Manager | -- can also add NormalUsers, inspect any data/history
  Administrator -- can also add Managers
  deriving (Show,Eq,Data,Typeable)

data User = User {
  handle :: Handle,
  password :: Password,
  role :: Role
  } deriving (Show,Eq,Data,Typeable)

canSeeUserList :: Role -> Bool
canSeeUserList Administrator = True
canSeeUserList Manager = True
canSeeUserList _ = False

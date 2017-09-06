module InnerEar.Types.User where

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Data

data User = User {
  handle :: Handle,
  password :: Password,
  canModifyUsers :: Bool
}

newUser :: Handle -> Password -> User
newUser h p = User {
  handle = h,
  password = p,
  canModifyUsers = False
}

-- addPoint :: Point -> User -> User
-- addPoint p u = u { points = p:(points u) }

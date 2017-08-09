module InnerEar.Types.User where

import qualified Network.WebSockets as WS

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Point

data User = User {
  handle :: Handle,
  password :: Password,
  points :: [Point]
}

newUser :: Handle -> Password -> User
newUser h p = User {
  handle = h,
  password = p,
  points = []
}

addPoint :: Point -> User -> User
addPoint p u = u { points = p:(points u) }

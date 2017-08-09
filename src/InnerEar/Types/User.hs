module InnerEar.Types.User where

import qualified Network.WebSockets as WS

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Point

data User = User {
  connection :: WS.Connection,
  authenticated :: Bool,
  handle :: Handle,
  password :: Password,
  points :: [Point]
}

newUser :: WS.Connection -> User
newUser c = User {
  connection = c,
  authenticated = False,
  handle = "",
  password = ""
  points = []
}

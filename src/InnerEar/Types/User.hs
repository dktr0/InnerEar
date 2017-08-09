module InnerEar.Types.User where

import qualified Network.WebSockets as WS

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Point

data User = User {
  handle :: Handle,
  password :: Password,
  authenticated :: Bool,
  points :: [Point]
}

newUser :: Handle -> Password -> User
newUser h p = User {
  handle = h,
  password = p,
  authenticated = False,
  points = []
}

authenticate :: User -> User
authenticate x = x { authenticated = True }

deauthenticate :: User -> User
deauthenticate x = x { authenticated = False }

module InnerEar.Types.Request where

import Text.JSON
import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Password

data Request =
  CreateUser Handle Password |
  Authenticate Handle Password

instance JSON Request where
  showJSON (CreateUser h p) = encJSDict [("CreateUser",h),("p",p)]
  showJSON (Authenticate h p) = encJSDict [("Authenticate",h),("p",p)]
  readJSON (JSObject x) | firstKey x == "CreateUser" = CreateUser <$> valFromObj "CreateUser" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x <*> valFromObj "p" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Request" ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Request"

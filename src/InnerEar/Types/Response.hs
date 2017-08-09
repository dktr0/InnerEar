module InnerEar.Types.Response where

import Text.JSON
import InnerEar.Types.Utility
import InnerEar.Types.Handle

data Response =
  Authenticated Handle -- signals that client is successfully authenticated as the indicated handle

instance JSON Response where
  showJSON (Authenticated h) = encJSDict [("Authenticated",h)]
  readJSON (JSObject x) | firstKey x == "Authenticated" = Authenticated <$> valFromObj "Authenticated" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Response" ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Response"

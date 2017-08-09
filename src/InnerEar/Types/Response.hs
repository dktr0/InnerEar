module InnerEar.Types.Response where

import Text.JSON
import InnerEar.Types.Utility
import InnerEar.Types.Handle
import InnerEar.Types.Record

data Response =
  Authenticated Handle | -- signals that client is successfully authenticated as the indicated handle
  Downloaded Record

instance JSON Response where
  showJSON (Authenticated h) = encJSDict [("Authenticated",h)]
  showJSON (Downloaded r) = encJSDict [("Downloaded",r)]
  readJSON (JSObject x) | firstKey x == "Authenticated" = Authenticated <$> valFromObj "Authenticated" x
  readJSON (JSObject x) | firstKey x == "Downloaded" = Downloaded <$> valFromObj "Downloaded" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Response" ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Response"

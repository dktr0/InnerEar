module InnerEar.Types.Record where

import Text.JSON
import InnerEar.Types.Point
import InnerEar.Types.Handle

data Record = Record {
  handle :: Handle,
  point :: Point
}

instance JSON Record where
  showJSON (Record h p) = encJSDict [("h",showJSON h),("p",showJSON p)]
  readJSON (JSObject x) = Record <$> valFromObj "h" x <*> valFromObj "p" x
  readJSON _ = Error "Unable to parse non-JSObject as Record"

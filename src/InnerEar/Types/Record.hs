module InnerEar.Types.Record where

import Text.JSON
import InnerEar.Types.Point
import InnerEar.Types.Handle

data Record = Record {
  handle :: Handle,
  point :: Point
}

instance JSON Record where
  showJSON (Record h p) = showJSON (h,p)
  readJSON (JSObject x) =
  readJSON _ = Error "Unable to parse non-JSObject as Record"

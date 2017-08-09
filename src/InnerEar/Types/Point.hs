module InnerEar.Types.Point where

import Data.Time.Clock
import InnerEar.Types.Datum

type Time = UTCTime

data Point = Point {
  time :: Time,
  datum :: Datum
}

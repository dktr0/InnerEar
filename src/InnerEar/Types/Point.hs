{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Point where

import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock
import InnerEar.Types.Utility
import InnerEar.Types.Datum

type Time = UTCTime

data Point = Point {
  time :: Time,
  datum :: Datum
} deriving (Eq,Data,Typeable)

instance Show Point where
  show (Point t d) = show d ++ " (warning: not showing time)"

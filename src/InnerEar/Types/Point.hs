{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Point where

import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock
import InnerEar.Types.Utility
import InnerEar.Types.Datum

type Time = UTCTime

data Point = Point {
  datum :: Datum,
  time :: Time
} deriving (Eq,Data,Typeable)

instance Show Point where
  show (Point d t) = show d ++ " (warning: not showing time)"

datumToPoint :: Datum -> IO Point
datumToPoint x = getCurrentTime >>= return . Point x

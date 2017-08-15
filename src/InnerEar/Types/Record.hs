{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Record where

import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Point
import InnerEar.Types.Handle

data Record = Record {
  handle :: Handle,
  point :: Point
} deriving (Show,Eq,Data,Typeable)

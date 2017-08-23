{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Frequency where

import Text.JSON
import Text.JSON.Generic

data Frequency = F {
  freqAsDouble :: Double,
  freqAsString :: String
} deriving (Data,Typeable)

instance Eq Frequency where
  (F _ x) == (F _ y) = x == y

instance Show Frequency where
  show = freqAsString

instance Ord Frequency where
  compare (F x _) (F y _) = compare x y

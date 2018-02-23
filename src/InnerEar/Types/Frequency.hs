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

instance Num Frequency where
  (F x y) + (F z _) = F (x+z) (show (x+z))
  (F x y) - (F z _) = F (x-z) (show (x-z))
  (F x y) * (F z _) = F (x*z) (show (x*z))
  abs (F x y) = F (abs x) (show (abs x))
  signum (F x y) | x > 0 = F 1 "1"
  signum (F x y) | x < 0 = F (-1) "-1"
  signum (F x y) | x == 0 = F 0 "0"
  fromInteger x = F (fromIntegral x) (show (fromIntegral x))

instance Enum Frequency where
  toEnum x = F (fromIntegral x) (show (fromIntegral x))
  fromEnum (F x _) = round x

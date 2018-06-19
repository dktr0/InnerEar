{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.GScore where

import Text.JSON
import Text.JSON.Generic
import Data.Map

data GScore = GScore {
  score :: Double,
  outOf :: Double
} deriving (Eq, Typeable, Data)

-- asPercent' :: GScore -> Double
-- asPercent (GScore 0 b) = 0.0
-- asPercent (GScore a b) = a/b

asPercent :: GScore -> Double
asPercent (GScore 0 b) = 0.0 :: Double
asPercent (GScore a b) = ((a/b)*100.0) :: Double

instance Show GScore where
   show (GScore a b) = show (round a) ++ "/" ++ show (round b)

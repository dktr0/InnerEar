{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.MultipleChoiceStore where

import Data.Map
import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Score

type XpFunction c a = Map c (Map a Score) -> (Int,Int)

scoreForConfig :: Ord c => Map c (Map a Score) -> c -> Double -> Double
scoreForConfig m c maxPoints = x * maxPoints
  where x = (Data.Map.foldl (+) 0 $ fmap asPercent $ findWithDefault empty c m)

data MultipleChoiceStore c a = MultipleChoiceStore {
  scores :: Map c (Map a Score),
  xp :: (Int,Int)
  } deriving (Show,Eq,Data,Typeable)

instance (Ord c, Ord a, Data c, Data a) => JSON (MultipleChoiceStore c a) where
  showJSON = toJSON
  readJSON = fromJSON

newStoreWithNoScores :: (Ord c, Ord a) => XpFunction c a -> MultipleChoiceStore c a
newStoreWithNoScores f = MultipleChoiceStore { scores = empty, xp = f empty }

newScores :: (Ord c, Ord a)
  => XpFunction c a
  -> (c, Map a Score -> Map a Score)
  -> MultipleChoiceStore c a -> MultipleChoiceStore c a
newScores xpF (c,newScoresF) s = s { scores = newMap, xp = xpF newMap }
  where
    oldSubMap = findWithDefault empty c $ scores s
    newSubMap = newScoresF oldSubMap
    newMap = insert c newSubMap $ scores s

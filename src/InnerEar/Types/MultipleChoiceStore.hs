{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.MultipleChoiceStore where

import Data.Map
import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Score

data MultipleChoiceStore c a = MultipleChoiceStore {
  scores :: Map c (Map a Score),
  xp :: (Int,Int)
  } deriving (Show,Eq,Typeable,Data)

newScores :: (Eq c, Eq a, Ord c, Ord a) => (Map c (Map a Score) -> (Int,Int)) -> Map c (Map a Score) -> MultipleChoiceStore c a -> MultipleChoiceStore c a
newScores f m s = s { scores = m, xp = f m }

newScoresForConfig :: (Eq c, Eq a, Ord c, Ord a) => (Map c (Map a Score) -> (Int,Int)) -> c -> Map a Score -> MultipleChoiceStore c a -> MultipleChoiceStore c a
newScoresForConfig f c m s = s { scores = newMap, xp = f newMap }
  where newMap = insert c m (scores s)


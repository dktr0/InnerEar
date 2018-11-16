{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.MultipleChoiceStore where

import Data.Map
import Text.JSON
import Text.JSON.Generic
import InnerEar.Types.Score

data MultipleChoiceStore c a = MultipleChoiceStore {
  recentAnswers :: [(c,Bool)], -- True for correct, False for incorrect
  scores :: Map c (Map a Score),
  xp :: (Int,Int)
  } deriving (Show,Eq,Data,Typeable)

instance (Ord c, Ord a, Data c, Data a) => JSON (MultipleChoiceStore c a) where
  showJSON = toJSON
  readJSON = fromJSON

type XpFunction c a = MultipleChoiceStore c a -> (Int,Int)

scoreForConfig :: Ord c => Map c (Map a Score) -> c -> Double -> Double
scoreForConfig m c maxPoints = x * maxPoints
  where x = (Data.Map.foldl (+) 0 $ fmap asPercent $ findWithDefault empty c m)

summedScoreForConfig :: Eq c => MultipleChoiceStore c a -> c -> Double -> Double
summedScoreForConfig s c value | length (recentAnswers s) == 0 = 0.0
summedScoreForConfig s c value | otherwise = (/ fromIntegral (length (recentAnswers s))) $ sum $ fmap (const value) $ Prelude.filter (\(x,y) -> x == c && y) (recentAnswers s)

newStoreWithNoScores :: (Ord c, Ord a) => XpFunction c a -> MultipleChoiceStore c a
newStoreWithNoScores f = MultipleChoiceStore { recentAnswers = [], scores = empty, xp = (0,0) }

newAnswer :: XpFunction c a
  -> (c,Bool)
  -> MultipleChoiceStore c a -> MultipleChoiceStore c a
newAnswer xpF x s = s' { xp = xpF s' }
  where s' = s { recentAnswers = take 20 $ (x:(recentAnswers s)) }

newScores :: (Ord c, Ord a)
  => XpFunction c a
  -> (c, Map a Score -> Map a Score)
  -> MultipleChoiceStore c a -> MultipleChoiceStore c a
newScores xpF (c,newScoresF) s = s' { xp = xpF s' }
  where
    s' = s { scores = newMap }
    oldSubMap = findWithDefault empty c $ scores s
    newSubMap = newScoresF oldSubMap
    newMap = insert c newSubMap $ scores s

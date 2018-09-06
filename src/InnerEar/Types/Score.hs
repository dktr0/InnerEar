{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Score where

import Text.JSON
import Text.JSON.Generic
import Data.Map

data Score = Score {
  correctAnswers :: Int,
  falsePositives::Int, -- times user thinks it is this the answer but correct answer is something else
  falseNegatives::Int  -- times user thinks it is another answer but this is the correct answer
} deriving (Show,Eq,Data,Typeable)

instance JSON Score where
  showJSON = toJSON
  readJSON = fromJSON

-- Times that the answer this score pertains to has been the answer
questionsAsked :: Score -> Int
questionsAsked (Score a _ c) = a+c

asPercent :: Score -> Double
asPercent s | questionsAsked s == 0 = 0.0
asPercent s | otherwise = (((fromIntegral . correctAnswers) s) :: Double) / (((fromIntegral . questionsAsked) s) :: Double )

incFalsePositive:: Score -> Score
incFalsePositive (Score a b c) = Score a (b+1) c

incFalseNegative:: Score -> Score
incFalseNegative (Score a b c) = Score a b (c+1)

incCorrect:: Score -> Score
incCorrect (Score a b c) = Score (a+1) b c

markCorrect :: Ord a => a -> Map a Score -> Map a Score
markCorrect rightAnswer = alter f rightAnswer
  where f (Just x) = Just $ incCorrect x
        f Nothing = Just $ Score 1 0 0

markIncorrect :: Ord a => a -> a -> Map a Score -> Map a Score
markIncorrect theirAnswer rightAnswer = alter falseP theirAnswer . alter falseN rightAnswer
  where falseP (Just x) = Just $ incFalsePositive x
        falseP Nothing = Just $ Score 0 1 0
        falseN (Just x) = Just $ incFalseNegative x
        falseN Nothing = Just $ Score 0 0 1

{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Score where


import Text.JSON
import Text.JSON.Generic

data Score = Score {
  questionsAsked::Int, -- times user has been tested on this option (times asked)
  falsePositives::Int, -- times user thinks it is this the answer but correct answer is something else
  falseNegatives::Int  -- times user thinks it is another answer but this is the correct answer
} deriving (Show, Eq,Typeable,Data)

data ScorePossibility = Correct | FalsePositive | FalseNegative deriving (Show, Eq)

adjustScore::ScorePossibility -> Score -> Score
adjustScore (Correct) (Score a b c) = Score (a+1) b c
adjustScore (FalsePositive) (Score a b c) = Score a (b+1) c
adjustScore (FalseNegative) (Score a b c) = Score (a+1) b (c+1)

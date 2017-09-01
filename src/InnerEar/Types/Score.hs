{-# LANGUAGE DeriveDataTypeable #-}

module InnerEar.Types.Score where


import Text.JSON
import Text.JSON.Generic
import Data.Map

data Score = Score {
  correctAnswers :: Int,
  falsePositives::Int, -- times user thinks it is this the answer but correct answer is something else
  falseNegatives::Int  -- times user thinks it is another answer but this is the correct answer
} deriving (Show, Eq,Typeable,Data)

-- Times that the answer this score pertains to has been
-- the answer
questionsAsked::Score -> Int
questionsAsked (Score a _ c) = a+c

incFalsePositive:: Score -> Score
incFalsePositive (Score a b c) = Score a (b+1) c

incFalseNegative:: Score -> Score
incFalseNegative (Score a b c) = Score a b (c+1)

incCorrect:: Score -> Score
incCorrect (Score a b c) = Score (a+1) b c


-- second param is to be interpreted as: (correctA, Either (inCorrectA) (correctA))
updateScore::(Ord k)=> (k,Either k k) -> Map k Score -> Map k Score

updateScore (a,(Left b)) = (insertWith falseN a (Score 0 0 1)) . (insertWith falseP b (Score 0 1 0))
  where falseN new old = incFalseNegative old
        falseP new old = incFalsePositive old

updateScore (_,(Right b)) = insertWith (\new old -> incCorrect old) b (Score 1 0 0)

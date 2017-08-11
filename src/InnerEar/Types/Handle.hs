module InnerEar.Types.Handle where

type Handle = String

isValidHandle :: Handle -> Bool
isValidHandle x = a && b && c
 where
   a = x != [] -- not empty
   b = take 1 x != " " -- first character is not a space
   c = take 1 (reverse x) != " " -- last character is not a space
   

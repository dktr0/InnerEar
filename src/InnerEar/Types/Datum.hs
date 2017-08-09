module InnerEar.Types.Datum where

import Text.JSON
import InnerEar.Types.Utility

data Datum =
  SessionStart |
  SessionEnd |
  Score Int
  deriving (Show)

instance JSON Datum where
  showJSON SessionStart = showJSON "SessionStart"
  showJSON SessionEnd = showJSON "SessionEnd"
  showJSON (Score x) = encJSDict [("Score",x)]
  readJSON (JSString x) | fromJSString x == "SessionStart" = Ok SessionStart
  readJSON (JSString x) | fromJSString x == "SessionEnd" = Ok SessionEnd
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Datum: " ++ (show x)
  readJSON (JSObject x) | firstKey x == "Score" = Score <$> valFromObj "score" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Datum: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject or JSString as Datum"

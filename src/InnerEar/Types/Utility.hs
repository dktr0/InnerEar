module InnerEar.Types.Utility where

import Text.JSON

-- firstKey is useful when writing instance definitions for the class JSON
-- it extracts the first key from a JavaScript object which can then be used in pattern matching

firstKey :: JSObject JSValue -> String
firstKey = fst . head . fromJSObject

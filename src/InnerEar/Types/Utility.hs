module InnerEar.Types.Utility where

import Text.JSON
import Data.List (find)

-- | firstKey is useful when writing instance definitions for the class JSON
-- it extracts the first key from a JavaScript object which can then be used in pattern matching
firstKey :: JSObject JSValue -> String
firstKey = fst . head . fromJSObject

-- | lastWithPredicate is useful when processing batches of responses that arrive
-- "simultaneously" to the client from the server.
lastWithPredicate :: (a -> Bool) -> [a] -> Maybe a
lastWithPredicate p = find p . reverse

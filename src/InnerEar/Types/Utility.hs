module InnerEar.Types.Utility where

import Text.JSON
import Data.List (find,elemIndex)

-- | firstKey is useful when writing instance definitions for the class JSON
-- it extracts the first key from a JavaScript object which can then be used in pattern matching
firstKey :: JSObject JSValue -> String
firstKey = fst . head . fromJSObject

-- | lastWithPredicate is useful when processing batches of responses that arrive
-- "simultaneously" to the client from the server.
lastWithPredicate :: (a -> Bool) -> [a] -> Maybe a
lastWithPredicate p = find p . reverse

-- replaces b in [b] at the same index that a is in [a]
replaceAtSameIndex::(Eq a) => a -> [a] -> b -> [b] -> [b]
replaceAtSameIndex k l mode = maybe id (\x->replaceAt x mode) index
    where
        index = elemIndex k l
        replaceAt n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

midicps :: Double -> Double
midicps x = 440 * (2**((x-69)/12))

ampdb :: Double -> Double
ampdb x = 20 * (logBase 10 x)

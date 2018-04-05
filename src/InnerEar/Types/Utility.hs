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

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

riseToRiseAndFall :: (Double -> Double) -> Double -> Double
riseToRiseAndFall f x | x <= 0.5 = f (x*2)
riseToRiseAndFall f x | x > 0.5 = f ((1-x)*2)

scaleDomain :: Double -> Double -> (Double -> Double) -> Double -> Double
scaleDomain d1 d2 f x = f $ linlin d1 d2 0.0 1.0 x

scaleRange :: Double -> Double -> (Double -> Double) -> Double -> Double
scaleRange r1 r2 f x = linlin 0.0 1.0 r1 r2 $ f x

linlin :: Double -> Double -> Double -> Double -> Double -> Double
linlin in1 in2 out1 out2 x = (x-in1)/(in2-in1)*(out2-out1)+out1

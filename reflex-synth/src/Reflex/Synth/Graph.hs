module Reflex.Synth.Graph where

data NodeType = Oscillator | Gain | Destination deriving (Show)

type Connection = Int

data Node = Node {
  nodeType :: NodeType,
  internalConnections :: [Connection],
  externalConnections :: [Connection]
  } deriving (Show)

data Graph a = Graph {
  nodes :: [Node],
  duration :: Maybe Double,
  supplement :: a
  } deriving (Show)

instance Functor Graph where
  fmap f (Graph ns d s) = Graph ns d (f s)

instance Applicative Graph where
  pure x = Graph [] Nothing x
  (Graph ns1 d1 f) <*> (Graph ns2 d2 x) = Graph (combineNodes ns1 ns2) (combineDurations d1 d2) $ f x

instance Monad Graph where
  x >>= f = Graph {
    nodes = combineNodes (nodes x) (nodes y),
    duration = combineDurations (duration x) (duration y),
    supplement = supplement y
    }
    where y = f (supplement x)

combineNodes :: [Node] -> [Node] -> [Node]
combineNodes xs ys = xs ++ fmap (translateExternalConnections (length xs)) ys

translateExternalConnections :: Int -> Node -> Node
translateExternalConnections n xs = xs { internalConnections = y ++ externalConnections xs, externalConnections = []}
  where y = fmap (+ n) $ internalConnections xs

combineDurations :: Maybe Double -> Maybe Double -> Maybe Double
combineDurations Nothing Nothing = Nothing
combineDurations Nothing (Just x) = Just x
combineDurations (Just x) Nothing = Just x
combineDurations (Just x) (Just y) = Just (max x y)

oscillator :: Graph Connection
oscillator = Graph {
  nodes = [Node Oscillator [] []],
  duration = Nothing,
  supplement = 0
  }

gain :: Connection -> Graph Connection
gain src = Graph {
  nodes = [Node Gain [] [src]],
  duration = Nothing,
  supplement = 0
  }

destination :: Connection -> Graph ()
destination src = Graph {
  nodes = [Node Destination [] [src]],
  duration = Nothing,
  supplement = ()
  }

setDuration :: Maybe Double -> Graph ()
setDuration x = Graph {
  nodes = [],
  duration = x,
  supplement = ()
  }

test :: Graph ()
test = oscillator >>= gain >>= destination

test2 :: Graph ()
test2 = do
  x <- oscillator
  y <- gain x
  destination y

-- test (above) doesn't work, but test2 does
-- the difference between test and test2 has to do with the associativity of the >>= operator (left associative)
-- so test = (oscillator >>= gain) >>= destination
-- but test2 = oscillator >>= (\x -> gain x >>= destination)
-- the model here violates one of the monad laws connected with associativity
 

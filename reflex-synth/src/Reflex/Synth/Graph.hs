module Reflex.Synth.Graph where

data NodeType = Oscillator | Gain deriving (Show)

data Graph =
  Source NodeType |
  Sink Graph |
  SourceSink NodeType Graph |
  EmptyGraph
  deriving (Show)

combineGraphs :: Graph -> Graph -> Graph
combineGraphs x@(Sink _) y = error $ "Sink can't be first: " ++ show x ++ " : " ++ show y
combineGraphs x y@(Source _) = error $ "Source can't be second: " ++ show x ++ " : " ++ show y
combineGraphs x (SourceSink t y) = SourceSink t (combineGraphs x y)
combineGraphs x (Sink y) = Sink (combineGraphs x y)
combineGraphs x EmptyGraph = x

data Synth a = Synth {
  graph :: Graph,
  supplement :: a
  }
  deriving (Show)

instance Functor Synth where
  fmap f x = x { supplement = f (supplement x) }

instance Applicative Synth where
  pure x = Synth { graph = EmptyGraph, supplement = x }
  (Synth g1 f) <*> (Synth g2 x) = Synth { graph = combineGraphs g1 g2, supplement = f x }

instance Monad Synth where
  (Synth g1 a) >>= f = Synth { graph = combineGraphs g1 g2, supplement = b}
    where (Synth g2 b) = f a

oscillator :: Synth ()
oscillator = Synth { graph = Source Oscillator, supplement = () }

gain :: Synth ()
gain = Synth { graph = SourceSink Gain EmptyGraph, supplement = () }

destination :: Synth ()
destination = Synth { graph = Sink EmptyGraph, supplement = () }

test :: Synth ()
test = oscillator >> gain >> destination

test2 :: Synth ()
test2 = do
  oscillator
  gain
  destination

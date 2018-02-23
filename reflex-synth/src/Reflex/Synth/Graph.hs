{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Synth.Graph where

-- See https://wiki.haskell.org/New_monads/MonadUnique for a simple monad transformer to support
-- generating unique values

import Control.Monad.State

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

evalUniqueT :: Monad m => UniqueT m a -> m a
evalUniqueT (UniqueT s) = evalStateT s 0

type Reference = Integer
type Env = [(Reference, Graph)]

data SourceSpec
  = SourceRef Reference
  | Constant
  | Oscillator
  deriving (Show)

data NodeSpec
  = SourceSinkRef Reference
  | Gain
  | Parallel [Graph]
  deriving (Show)

data Graph
  = Source SourceSpec
  | Sink Graph
  | SourceSink NodeSpec Graph
  | Join [Graph] Graph -- Join xs y merges xs with unconnected inputs being connected to y
  | EmptyGraph
  deriving (Show)

connectGraphs :: Graph -> Graph -> Graph
connectGraphs EmptyGraph y = y
connectGraphs x EmptyGraph = x
connectGraphs x@(Sink _) y = error $ "Sink can't be first: " ++ show x ++ " : " ++ show y
connectGraphs x y@(Source _) = error $ "Source can't be second: " ++ show x ++ " : " ++ show y
connectGraphs x (SourceSink t y) = SourceSink t (connectGraphs x y)
connectGraphs x (Join gs y) = Join gs (connectGraphs x y)
connectGraphs x (Sink y) = Sink (connectGraphs x y)

data Synth a = Synth {
    graph :: Graph,
    env :: Env,
    forks :: [Synth ()],
    supplement :: a
  } deriving (Show)

instance Functor Synth where
  fmap f x = x { supplement = f (supplement x) }

instance Applicative Synth where
  pure x = Synth { graph = EmptyGraph, env = [], forks = [], supplement = x }
  (Synth g1 e1 fs1 f) <*> (Synth g2 e2 fs2 x) = Synth { 
    graph = connectGraphs g1 g2, 
    env = e1 ++ e2, -- TODO use a JS hash? for these references
    forks = fs1 ++ fs2,
    supplement = f x
  }

instance Monad Synth where
  (Synth g1 e1 fs1 a) >>= f = Synth { 
    graph = connectGraphs g1 g2,
    env = e1 ++ e2, 
    forks = fs1 ++ fs2, -- TODO fmap (connectGraphs g1) fs2 only on code gen
    supplement = b
  }
    where (Synth g2 e2 fs2 b) = f a

type SynthBuilder = UniqueT Synth

buildSynth :: SynthBuilder a -> Synth a
buildSynth = evalUniqueT

synthOfGraph :: Graph -> a -> SynthBuilder a
synthOfGraph g s = lift $ Synth { graph = g, env = [], forks = [], supplement = s }

oscillator :: SynthBuilder ()
oscillator = synthOfGraph (Source Oscillator) ()

gain :: SynthBuilder ()
gain = synthOfGraph (SourceSink Gain EmptyGraph) ()

destination :: SynthBuilder ()
destination = synthOfGraph (Sink EmptyGraph) ()

split :: Int -> [SynthBuilder Int]
split i = fmap createChannelSource [0..i-1]
  where
    createChannelSource x = synthOfGraph EmptyGraph x

-- |parallel is a closed fork-join that works on the channels of the input.
parallel :: Int ->  (Int -> SynthBuilder ()) -> SynthBuilder ()
parallel i f = lift $ Synth {
    graph = SourceSink (Parallel (fmap (\x -> graph (buildSynth (x >>= f))) (split i))) EmptyGraph,
    env = foldl (\x y -> x ++ env (buildSynth (y >>= f))) [] (split i),
    forks = [],
    supplement = ()
}

ref :: SynthBuilder () -> SynthBuilder Graph
ref r = do
  let (Synth g e fs _) = buildSynth r in do
    uid <- fresh
    lift $ Synth {
      graph = EmptyGraph,
      env = e ++ [(uid, g)],
      forks = fs,
      supplement =
        case g of
          (Source _) -> Source (SourceRef uid)
          (SourceSink _ g') -> SourceSink (SourceSinkRef uid) g'
          _ -> error $ "Cannot reference " ++ show g
    }

deref :: Graph -> SynthBuilder ()
deref g = synthOfGraph g ()

-- |diverge branches away from the main synth but retains references from the
-- original. Divergent forks must eventually connect to a destination which may
-- be the same destination as the top level synth (AudioContext).
diverge :: SynthBuilder () -> SynthBuilder ()
diverge forkBuilder = 
  let fork = buildSynth forkBuilder in do
    lift $ Synth {
      graph = EmptyGraph,
      env = [],
      forks = [fork],
      supplement = ()
    }

branch :: SynthBuilder () -> SynthBuilder (SynthBuilder ())
branch branchBuilder =
  let (Synth g e fs _) = buildSynth branchBuilder in do
    return $ lift $ Synth {
      graph = g,
      env = e,
      forks = fs,
      supplement = ()
    }

-- |merge is the opposite of `diverge` which merges the nodes.
merge :: [SynthBuilder ()] -> SynthBuilder ()
merge branchBuilders = 
  let branches = fmap buildSynth branchBuilders in do
    lift $ Synth {
      graph = Join (fmap graph branches) EmptyGraph,
      env = foldl (\x y -> x ++ (env y)) [] branches,
      forks = foldl (\x y -> x ++ (forks y)) [] branches,
      supplement = ()
    }

mix :: [SynthBuilder ()] -> SynthBuilder ()
mix synths =
  merge $ fmap (\x -> branch x >>= id) synths 

test :: Synth ()
test = buildSynth $ oscillator >> gain >> destination

test2 :: Synth ()
test2 = buildSynth $ do
  oscillator
  gain
  destination

test3 :: Synth ()
test3 = buildSynth $ do
  oscillator
  parallel 2 $ \c -> do
    if c == 0
      then gain
      else return ()
  destination

test4 :: Synth ()
test4 = buildSynth $ do
  osc <- ref oscillator
  deref osc
  destination

test5 :: Synth ()
test5 = buildSynth $ do
  lfo <- ref oscillator
  g <- ref gain
  oscillator
  diverge $ do
    deref lfo
    gain
    destination
  diverge $ do
    deref lfo
    deref g
    destination
  gain
  destination

test6 :: Synth ()
test6 = buildSynth $ do
  gGain <- ref gain
  oscillator
  g <- branch $ do
    deref gGain
    gain
  h <- branch $ gain
  merge [g, h]
  gain
  destination

test7 :: Synth ()
test7 = buildSynth $ do
  oscillator
  mix $ replicate 3 gain

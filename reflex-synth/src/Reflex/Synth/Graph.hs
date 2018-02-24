{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Synth.Graph (
  Synth,
  SynthBuilder,
  buildSynth,
  ref,
  deref,
  parallelChannels,
  branch,
  merge,
  mix,
  setParamValue,
  linearRampToParamValue,
  exponentialRampToParamValue,
  curveToParamValue
) where

import qualified Data.Map as Map
import Control.Monad.State

-- See https://wiki.haskell.org/New_monads/MonadUnique for a simple monad transformer to support
-- generating unique values.

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
type Env = Map.Map Reference Graph

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
  | Fork Graph Graph -- The first graph may have its own source in which case this is a loose diverge
  | EmptyGraph
  deriving (Show)

type AudioParam = String

data Change
  = SetValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Double }
  | LinearRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Double }
  | ExponentialRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Double }
  | CurveToValue { node :: Graph, param :: AudioParam, values :: [Double], startTime :: Double, duration :: Double }
  deriving (Show)

connectGraphs :: Graph -> Graph -> Graph
connectGraphs EmptyGraph y = y
connectGraphs x EmptyGraph = x
connectGraphs x@(Sink _) y = error $ "Sink can't be first: " ++ show x ++ " : " ++ show y
connectGraphs x y@(Source _) = error $ "Source can't be second: " ++ show x ++ " : " ++ show y
connectGraphs x (SourceSink t y) = SourceSink t (connectGraphs x y)
connectGraphs x (Join gs y) = Join gs (connectGraphs x y)
connectGraphs x (Fork br y) = Fork br (connectGraphs x y)
connectGraphs x (Sink y) = Sink (connectGraphs x y)

graphSource :: Graph -> Graph
graphSource g@(Source _) = g
graphSource (Sink input) = graphSource input
graphSource (SourceSink _ input) = graphSource input
graphSource (Join _ input) = graphSource input
graphSource (Fork _ input) = graphSource input
graphSource EmptyGraph = EmptyGraph

data Synth a = Synth {
    graph :: Graph,
    env :: Env,
    changes :: [Change],
    supplement :: a
  } deriving (Show)

instance Functor Synth where
  fmap f x = x { supplement = f (supplement x) }

instance Applicative Synth where
  pure x = Synth { graph = EmptyGraph, env = Map.empty, changes = [], supplement = x }
  (Synth g1 e1 cs1 f) <*> (Synth g2 e2 cs2 x) = Synth { 
    graph = connectGraphs g1 g2, 
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    supplement = f x
  }

instance Monad Synth where
  (Synth g1 e1 cs1 a) >>= f = Synth { 
    graph = connectGraphs g1 g2,
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    supplement = b
  } where (Synth g2 e2 cs2 b) = f a

type SynthBuilder = UniqueT Synth

buildSynth :: SynthBuilder a -> Synth a
buildSynth = evalUniqueT

synthOfGraph :: Graph -> a -> SynthBuilder a
synthOfGraph g s = lift $ Synth { graph = g, env = Map.empty, changes = [], supplement = s }

oscillator :: SynthBuilder ()
oscillator = synthOfGraph (Source Oscillator) ()

gain :: SynthBuilder ()
gain = synthOfGraph (SourceSink Gain EmptyGraph) ()

destination :: SynthBuilder ()
destination = synthOfGraph (Sink EmptyGraph) ()

splitChannels :: Int -> [SynthBuilder Int]
splitChannels i = fmap createChannelSource [0..i-1]
  where
    createChannelSource x = synthOfGraph EmptyGraph x

-- |parallel is a closed fork-join that works on the channels of the input.
parallelChannels :: Int ->  (Int -> SynthBuilder ()) -> SynthBuilder ()
parallelChannels i f = lift $ Synth { -- TODO pass supplement through
    graph = SourceSink (Parallel (fmap (\x -> graph (buildSynth (x >>= f))) (splitChannels i))) EmptyGraph,
    env = foldl (\x y -> Map.union x $ env (buildSynth (y >>= f))) Map.empty (splitChannels i),
    changes = foldl (\x y -> x ++ changes (buildSynth (y >>= f))) [] (splitChannels i),
    supplement = ()
}

ref :: SynthBuilder a -> SynthBuilder Graph
ref r = do
  let (Synth g e cs _) = buildSynth r in do
    uid <- fresh
    lift $ Synth {
      graph = EmptyGraph,
      env = Map.insert uid g e,
      changes = cs,
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
diverge :: SynthBuilder a -> SynthBuilder a
diverge forkBuilder =
  let (Synth g e cs s) = buildSynth forkBuilder in do
    lift $ Synth {
      graph = Fork g EmptyGraph,
      env = e,
      changes = cs,
      supplement = s
    }

branch :: SynthBuilder a -> SynthBuilder (SynthBuilder a)
branch branchBuilder =
  let (Synth g e cs s) = buildSynth branchBuilder in do
    return $ lift $ Synth {
      graph = g,
      env = e,
      changes = cs,
      supplement = s
    }

-- |merge is the opposite of `diverge` which merges the nodes.
merge :: [SynthBuilder a] -> SynthBuilder [a]
merge branchBuilders =
  let branches = fmap buildSynth branchBuilders in do
    lift $ Synth {
      graph = Join (fmap graph branches) EmptyGraph,
      env = foldl (\x y -> Map.union x $ env y) Map.empty branches,
      changes = foldl (\x y -> x ++ (changes y)) [] branches,
      supplement = fmap supplement branches
    }

mix :: [SynthBuilder a] -> SynthBuilder [a]
mix synths =
  merge $ fmap (\x -> branch x >>= id) synths

change :: Change -> SynthBuilder ()
change c = lift $ Synth {
  graph = EmptyGraph,
  env = Map.empty,
  changes = [c],
  supplement = ()
}

setParamValue :: Graph -> AudioParam -> Double -> Double -> SynthBuilder ()
setParamValue n p v e = change $ SetValue n p v e

linearRampToParamValue :: Graph -> AudioParam -> Double -> Double -> SynthBuilder ()
linearRampToParamValue n p v e = change $ LinearRampToValue n p v e

exponentialRampToParamValue :: Graph -> AudioParam -> Double -> Double -> SynthBuilder ()
exponentialRampToParamValue n p v e = change $ ExponentialRampToValue n p v e

curveToParamValue :: Graph -> AudioParam -> [Double] -> Double -> Double -> SynthBuilder ()
curveToParamValue n p vs s d = change $ CurveToValue n p vs s d

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
  parallelChannels 2 $ \c -> do
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
  return ()

test8 :: Synth ()
test8 = buildSynth $ do
  g <- ref gain
  setParamValue g "gain" 0.0 1.0
  
test9 :: Synth ()
test9 = buildSynth $ do
  oscillator
  diverge $ do
    gain
    destination
  gain

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Synth.Graph (
  SourceProps,
  SourceSinkProps,
  SinkProps,
  Graph,
  Change,
  Synth,
  SynthBuilder,
  Reference,
  Env,
  buildSynth,
  synthSource,
  synthNode,
  synthSink,
  audioParamSink,
  ref,
  deref,
--  parallelChannels,
--  diverge,
--  branch,
--  merge,
--  mix,
  setParamValue,
  linearRampToParamValue,
  exponentialRampToParamValue,
  curveToParamValue
) where

import qualified Data.Map as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Reflex.Synth.Spec

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

data SourceProps
  = SourceRef Reference
  | SourceSpec SourceNodeSpec
  deriving (Show)

data SourceSinkProps
  = SourceSinkRef Reference
  | Parallel [Graph]
  | SourceSinkSpec NodeSpec
  deriving (Show)

data SinkProps
  = SinkParamRef Graph AudioParam
  | SinkSpec SinkNodeSpec
  deriving (Show)

data Graph
  = Source SourceProps
  | Sink SinkProps Graph
  | SourceSink SourceSinkProps Graph
  | EmptyGraph
  deriving (Show)

type AudioParam = String

data Change
  = SetValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | LinearRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | ExponentialRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | CurveToValue { node :: Graph, param :: AudioParam, values :: [Double], startTime :: Time, duration :: Time }
  deriving (Show)

data Synth a = Synth {
    graphs :: ([Graph], [Graph]),
    env :: Env,
    changes :: [Change],
    supplement :: a
  } deriving (Show)

type SynthBuilder = UniqueT Synth

buildSynth :: SynthBuilder a -> Synth a
buildSynth = evalUniqueT

connectGraphs :: Graph -> Graph -> Graph
connectGraphs EmptyGraph y = y
connectGraphs x EmptyGraph = x
connectGraphs x@(Sink _ _) y = error $ "Sink can't be first: " ++ show x ++ " : " ++ show y
connectGraphs x y@(Source _) = error $ "Source can't be second: " ++ show x ++ " : " ++ show y
connectGraphs x (SourceSink spec y) = SourceSink spec (connectGraphs x y)
connectGraphs x (Sink spec y) = Sink spec (connectGraphs x y)

connectGraphs' :: ([Graph], [Graph]) -> Graph -> ([Graph], [Graph])
connectGraphs' ([], completed) y
  | isComplete y = ([], y:completed) -- Already completed
  | otherwise = ([y], completed) -- Incomplete, push to working stack
connectGraphs' (x@(Sink _ _):tl, completed) y@(Sink _ _)
  | isComplete y = (x:tl, y:completed)
  | otherwise = (y:x:tl, completed)
connectGraphs' (hd:tl, completed) y
  | isComplete y = (hd:tl, y:completed)
  | hasSource y  = (y:hd:tl, completed)
  | otherwise = if isComplete connected
    then (tl, connected:completed)
    else (connected:tl, completed)
  where connected = connectGraphs hd y

connectGraphStacks :: ([Graph], [Graph]) -> ([Graph], [Graph]) -> ([Graph], [Graph])
connectGraphStacks (incomp1, comp1) ([], comp2) = (incomp1, comp1 ++ comp2)
connectGraphStacks stacks (hd:tl, complete) =
  connectGraphStacks (connectGraphs' stacks hd) (tl, complete)

isComplete :: Graph -> Bool
isComplete (Sink _ x) = hasSource x
isComplete _ = False

getSource :: Graph -> Graph
getSource x@(Source _) = x
getSource (Sink _ x) = getSource x
getSource (SourceSink _ x) = getSource x
getSource EmptyGraph = EmptyGraph

hasSource :: Graph -> Bool
hasSource g = case getSource g of
  Source _ -> True
  _ -> False

instance Functor Synth where
  fmap f x = x { supplement = f (supplement x) }

instance Applicative Synth where
  pure x = Synth { graphs = ([], []), env = Map.empty, changes = [], supplement = x }
  (Synth g1 e1 cs1 f) <*> (Synth g2 e2 cs2 x) = Synth {
    graphs = connectGraphStacks g1 g2,
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    supplement = f x
  }

instance Monad Synth where
  (Synth g1 e1 cs1 a) >>= f = Synth {
    graphs = connectGraphStacks g1 g2,
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    supplement = b
  } where (Synth g2 e2 cs2 b) = f a

synthOfGraph :: Graph -> a -> SynthBuilder a
synthOfGraph g s = lift $ Synth {
  graphs = if isComplete g then ([], [g]) else ([g], []),
  env = Map.empty,
  changes = [],
  supplement = s
}

synthSource :: SourceNodeSpec -> SynthBuilder ()
synthSource spec = synthOfGraph (Source $ SourceSpec spec) ()

synthNode :: NodeSpec -> SynthBuilder ()
synthNode spec = synthOfGraph (SourceSink (SourceSinkSpec spec) EmptyGraph) ()

synthSink :: SinkNodeSpec -> SynthBuilder ()
synthSink spec = synthOfGraph (Sink (SinkSpec spec) EmptyGraph) ()

audioParamSink :: Graph -> AudioParam -> SynthBuilder ()
-- TODO only match references here? It doesn't make sense to modulate an isolated node
audioParamSink g p = synthOfGraph (Sink (SinkParamRef g p) EmptyGraph) ()

splitChannels :: Int -> [SynthBuilder Int]
splitChannels i = fmap createChannelSource [0..i-1]
  where
    createChannelSource x = synthOfGraph EmptyGraph x

-- |parallel is a closed fork-join that works on the channels of the input.
--parallelChannels :: Int ->  (Int -> SynthBuilder ()) -> SynthBuilder ()
--parallelChannels i f = lift $ Synth { -- TODO pass supplement through
--    graph = SourceSink (Parallel (fmap (\x -> graph (buildSynth (x >>= f))) (splitChannels i))) EmptyGraph,
--    env = foldl (\x y -> Map.union x $ env (buildSynth (y >>= f))) Map.empty (splitChannels i),
--    changes = foldl (\x y -> x ++ changes (buildSynth (y >>= f))) [] (splitChannels i),
--    supplement = ()
--}

ref :: SynthBuilder a -> SynthBuilder Graph
ref r = do
  let (Synth (g:_, []) e cs _) = buildSynth r in do
    uid <- fresh
    let reference = case g of
          (Source _) -> Source (SourceRef uid)
          (SourceSink _ g') -> SourceSink (SourceSinkRef uid) g'
          _ -> error $ "Cannot reference " ++ show g
    synthOfGraph reference ()
    lift $ Synth {
      graphs = ([], []),
      env = Map.insert uid g e,
      changes = cs,
      supplement = reference
    }

deref :: Graph -> SynthBuilder ()
deref g = synthOfGraph g ()

-- |diverge branches away from the main synth but retains references from the
-- original. Divergent forks must eventually connect to a destination which may
-- be the same destination as the top level synth (AudioContext).
--diverge :: SynthBuilder a -> SynthBuilder a
--diverge forkBuilder =
--  let (Synth g e cs s) = buildSynth forkBuilder in do
--    lift $ Synth {
--      graph = Unconnected g EmptyGraph,
--      env = e,
--      changes = cs,
--      supplement = s
--    }

--branch :: SynthBuilder a -> SynthBuilder (SynthBuilder a)
--branch branchBuilder =
--  let (Synth g e cs s) = buildSynth branchBuilder in do
--    return $ lift $ Synth {
--      graph = g,
--      env = e,
--      changes = cs,
--      supplement = s
--    }

-- |merge is the opposite of `diverge` which merges the nodes.
--merge :: [SynthBuilder a] -> SynthBuilder [a]
--merge branchBuilders =
--  let branches = fmap buildSynth branchBuilders in do
--    lift $ Synth {
--      graph = Join (fmap graph branches) EmptyGraph,
--      env = foldl (\x y -> Map.union x $ env y) Map.empty branches,
--      changes = foldl (\x y -> x ++ (changes y)) [] branches,
--      supplement = fmap supplement branches
--    }

--mix :: [SynthBuilder a] -> SynthBuilder [a]
--mix synths =
--  merge $ fmap (\x -> branch x >>= id) synths

change :: Change -> SynthBuilder ()
change c = lift $ Synth {
  graphs = ([], []),
  env = Map.empty,
  changes = [c],
  supplement = ()
}

setParamValue :: Graph -> AudioParam -> Double -> Time -> SynthBuilder ()
setParamValue n p v e = change $ SetValue n p v e

linearRampToParamValue :: Graph -> AudioParam -> Double -> Time -> SynthBuilder ()
linearRampToParamValue n p v e = change $ LinearRampToValue n p v e

exponentialRampToParamValue :: Graph -> AudioParam -> Double -> Time -> SynthBuilder ()
exponentialRampToParamValue n p v e = change $ ExponentialRampToValue n p v e

curveToParamValue :: Graph -> AudioParam -> [Double] -> Time -> Time -> SynthBuilder ()
curveToParamValue n p vs s d = change $ CurveToValue n p vs s d

-- test1 :: ([Graph], [Graph])
test1 :: Synth ()
test1 = buildSynth $ do
  r <- ref $ synthSource $ Oscillator Sine (Hz 440)
  linearRampToParamValue r "freq" 880 (Sec 4.0) 
  synthSource $ Oscillator Sine (Hz 220)
  synthSink Destination
  synthSink Destination
  synthSource $ Oscillator Sawtooth (Hz 110)
  synthSink Destination

test2 :: Synth ()
test2 = buildSynth $ do
  g <- ref $ synthSource $ Oscillator Sine (Hz 440)
  return ()

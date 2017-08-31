module Reflex.Synth.Sound where

data Sound a = Sound {
  graphDef :: GraphDef,
  duration :: Maybe Double,
  value :: a
  }

instance Monad (Sound a) where
  (Sound graphDefA durationA valueA) >>= f = Sound {
    graphDef = combineGraphDefs graphDefA graphDefB,
    duration = resolveDuration durationA durationB,
    value = valueB
    }
    where (Sound graphDefB durationB valueB) = f valueA
  (Sound graphDefA durationA _) >> (Sound graphDefB durationB valueB) = Sound {
    graphDef = combineGraphs graphA graphB,
    duration = resolveDuration durationA durationB,
    value = valueB
    }
  return x = Sound {
    graphDef = [],
    duration = Nothing,
    value = x
    }

resolveDuration :: Maybe Double -> Maybe Double -> Maybe Double
resolveDuration (Just x) Nothing = Just x
resolveDuration Nothing (Just x) = Just x
resolveDuration Nothing Nothing = Nothing
resolveDuration (Just x) (Just y) = Just y

setDuration :: Maybe Double -> Sound ()
setDuration x = Sound {
  graph = [],
  duration = x,
  value = ()
  }

performSound :: Sound a -> IO ()
performSound s = instantiateGraph (graphDef s) >>= startGraph

performSoundEvent :: MonadWidget t m => Event t (Sound a) -> m ()
performSoundEvent x = performEvent_ $ fmap (liftIO . performIO) x


-- work in progress

myAdditiveSound :: Sound ()
myAdditiveSound = do
  xs <- mapM sineWave [100,200,400,800,1600,3200,6400] -- Sound [GraphDef]
  y <- mix xs -- Sound GraphDef

gain :: Double -> Sound GraphDef
gain g = Sound {
  graphDef = [x],
  duration = Nothing,
  value = x
  }
  where x = gainNodeDef g

mixer :: [NodeDef] -> Sound NodeDef
mixer g = do
  gNode <- gain g
  mapM (connect _ gNode)

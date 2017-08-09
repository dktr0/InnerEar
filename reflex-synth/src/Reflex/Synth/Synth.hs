module Reflex.Synth.Synth where

import Reflex.Synth.Types 
--import InnerEar.Types.Sound
import qualified Reflex.Synth.Foreign as F
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)

class WebAudio a where
  createGraph :: a -> IO WebAudioGraph

data Filter = NoFilter |
              PeakingFilter Double Double Double -- Frequency Q Gain

type Duration = Double
data Source = PinkNoise Duration | Tone Oscillator Duration

data Sound = NoSynth | FilteredSound Source Filter



instance WebAudio Filter where
  createGraph (NoFilter) = createGain 1.0 >>= return . WebAudioGraph
  createGraph (PeakingFilter f q g) = createPeakingFilter f q g >>= return . WebAudioGraph



instance WebAudio Source where
  createGraph (PinkNoise dur) = do
    x <- createPinkNoise
    y <- createAsrEnvelope 0.005 dur 0.005 
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph
  createGraph (Tone osc dur) = do
    x <- createSaw
    y <- createAsrEnvelope 0.005 dur 0.005
    let graph = WebAudioGraph' x (WebAudioGraph y)
    createGraph graph


instance WebAudio WebAudioGraph where
  createGraph = connectGraph

instance WebAudio Sound where
  createGraph (FilteredSound s f) = do
    source <- createGraph s
    filt <- createGraph f
    let graph = WebAudioGraph'' source filt
    connectGraph graph


--createNode:: Source -> IO (WebAudioNode)
--createNode (PinkNoise dur) = do 
--  noise <- createPinkNoise
--  env <- createAsrEnvelope 0.05 dur 0.05
--  connect noise env


--instance WebAudio Synth where
--  createGraph (NoSynth) = return $ WebAudioGraph NullAudioNode
--  createGraph (Synth g d) = 

--instance WebAudio Synth where
--  createGraph (NoSynth) = return $ WebAudioGraph NullAudioNode
--  createGraph (FilteredSound s f) = do 
--    source <- createGraph 
--    filt <- createGraph f
--    connectGraphs source filt
--  createGraph (Synth s f) = do
--    let dur = case s of (PinkNoise a) -> a; otherwise -> 1 -- 1s default node length
--    x <- createGraph s
--    y <- createGraph f
--    env <- createAsrEnvelope 0.05 dur 0.05
--    dest <- getDestination
--    let graph = WebAudioGraph' x (WebAudioGraph' y (WebAudioGraph' env (WebAudioGraph dest)))
--    connectGraph graph
    
    --connect x y
    --connect y env
    --connectGraph env dest
    --return $ WebAudioGraph' s (WebAudioGraph' f (WebAudioGraph env))


--data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode

createAudioContext::IO ()
createAudioContext = F.createAudioContext

createSound :: Sound -> IO (WebAudioGraph)
createSound (FilteredSound s f) = do
  sourceNode <- createGraph s
  filterNode <- createGraph f
  dest <- getDestination
  let graph = WebAudioGraph'' sourceNode (WebAudioGraph'' filterNode (WebAudioGraph dest))
  connectGraph graph

performSynth:: MonadWidget t m => Event t Sound -> m ()
performSynth event = do
  let n = fmap (\e-> do 
                      graph <- createGraph e
                      startGraph graph
                      ) event          -- Event t (IO ())
  performEvent_ $ fmap liftIO n



--main::MonadWidget t m => IO()
--main = do 
--  F.createAudioContext
--  n <- createPinkNoise
--  dest <-F.getDestination
--  connect n dest
--  startNode n


  --createNode :: a -> IO WebAudioNode


--startNode :: WebAudioNode -> IO ()

--createNode :: a -> IO WebAudioNode

--play::IO WebAudioNode -> IO WebAudioNode

--performEvent_ :: MonadWidget t m => Event t (WidgetHost m ()) -> m ()

--iftIO :: MonadIO m => IO a -> m a

--doHint :: WebDirt -> Hint -> IO ()

--performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
--performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev



-- an example of how this might be used with reflex-dom:

--example :: m (Event t ())
--example = do
--  play <- button "play"
--  let synthAction = createNode (Synth PinkNoise (PeakingFilter 1500 1.4 6.0)) <$ play
--  performEvent_ $ fmap liftIO synthAction

--getNewQuestion :: StdGen -> IO (Synth,StdGen)
--getNewQuestion gen = do
--  (x,g) <- randomR (0,1) gen
--  let s = if x==0 then (Synth PinkNoise NoFilter) else (Synth PinkNoise (PeakingFilter 1500 1.4 6.0))
--  return (s,g)

--example2 :: StdGen -> m ()
--example2 gen = el "div" $ mdo
--  newButton <- button "new"
--  gen'' <- tagPromptlyDyn gen' newButton -- m (Event t StdGen)
--  newQuestion <- performEvent $ fmap (liftIO . getNewQuestion) gen'' -- m (Event t (Synth,StdGen))
--  synth <- holdDyn (NoSynth) $ fmap fst newQuestion -- m (Dynamic t Synth)
--  gen' <- holdDyn gen $ fmap snd newQuestion -- m (Dynamic t StdGen)
--  playButton <- button "play"
--  synthAction <- tagPromptlyDyn synth playButton -- m (Event t Synth)
--  performEvent_ $ fmap (liftIO . createNode) synthAction

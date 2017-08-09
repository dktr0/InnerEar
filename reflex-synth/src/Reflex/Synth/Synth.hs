module Reflex.Synth.Synth where

import Reflex.Synth.Types 
import qualified Reflex.Synth.Foreign as F
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)

class WebAudio a where
  createNode :: a -> IO WebAudioNode

data Filter = NoFilter |
              PeakingFilter Double Double Double -- Frequency Q Gain

instance WebAudio Filter where
  createNode (NoFilter) = createGain 1.0
  createNode (PeakingFilter f q g) = createPeakingFilter f q g

type Duration = Double

data Source = PinkNoise Duration

instance WebAudio Source where
  createNode (PinkNoise dur) = do
    x <- createPinkNoise
    y <- createAsrEnvelope 0.005 dur 0.005 
    connect x y
    return x


data Synth = NoSynth | Synth Source Filter

instance WebAudio WebAudioGraph where
  createNode = connectGraph'

instance WebAudio Synth where
  createNode (NoSynth) = return NullAudioNode
  createNode (Synth s f) = do
    let dur = case s of (PinkNoise a) -> a; otherwise -> 1 -- 1s default node length
    x <- createNode s
    y <- createNode f
    env <- createAsrEnvelope 0.05 dur 0.05
    dest <- getDestination
    connect x y
    connect y env
    connect env dest
    return env


--data WebAudioNode = WebAudioNode NodeType JSVal | NullAudioNode



performSynth:: MonadWidget t m => Event t Synth -> m ()
performSynth event = do
  let n = fmap (\e-> do 
                      node <- createNode e
                      startNode node
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

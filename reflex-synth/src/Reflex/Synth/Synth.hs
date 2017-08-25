module Reflex.Synth.Synth where

import Reflex.Synth.Types 
--import InnerEar.Types.Sound
import qualified Reflex.Synth.Foreign as F
import qualified Data.Map as M
import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import GHCJS.DOM.JSFFI.Generated.HTMLElement
import GHCJS.DOM.FileReader (newFileReader,getResult, readAsDataURL,load)
import GHCJS.DOM.EventM(on)
import GHCJS.DOM.Types(toJSString)
import qualified GHCJS.DOM.Types as G
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal(fromJSVal)


class WebAudio a where
  createGraph :: a -> IO WebAudioGraph

instance WebAudio Filter where
  createGraph (NoFilter) = createGain 1.0 >>= return . WebAudioGraph
  createGraph filt = createBiquadFilter filt >>= return . WebAudioGraph

instance WebAudio Buffer where
  createGraph (File path) = createBufferNode (File path) >>= return . WebAudioGraph

instance WebAudio Source where
  createGraph (NodeSource node dur) = 
    case node of
      (MediaNode a) ->do
        v<-createMediaNode a
        let graph = WebAudioGraph v
        createGraph graph
      (Destination) -> error "Destination cannot be a source node"
      (GainNode _) -> error "GainNode cannot be a source node"
      (FilterNode _) -> error "FilterNode cannot be a source node"
      otherwise -> do
        x <- createNode node
        y <- createAsrEnvelope 0.005 dur 0.005
        let graph = WebAudioGraph' x (WebAudioGraph y)
        createGraph graph

instance WebAudio WebAudioGraph where
  createGraph = connectGraph

instance WebAudio Sound where
  createGraph (Sound s) = do
    graph <- createGraph s
    connectGraph graph
    return graph
  createGraph (FilteredSound s f) = do
    source <- createGraph s
    filt <- createGraph f
    let graph = WebAudioGraph'' source filt
    connectGraph graph

createAudioContext::IO ()
createAudioContext = F.createAudioContext

createSound :: Sound -> IO (WebAudioGraph)
createSound (FilteredSound s f) = do
  sourceNode <- createGraph s
  filterNode <- createGraph f
  dest <- getDestination
  let graph = WebAudioGraph'' sourceNode (WebAudioGraph'' filterNode (WebAudioGraph dest))
  connectGraph graph

performSound:: MonadWidget t m => Event t Sound -> m ()
performSound event = do
  let n = fmap (\e-> do 
                      graph <- createGraph e
                      startGraph graph
                      ) event          -- Event t (IO ())
  performEvent_ $ fmap liftIO n

-- need to create to play the user's entered soundfile. needs to use the correct 'id'
-- (exaclty 'userAudio') to work
audioElement::MonadWidget t m => m ()
audioElement = elDynAttr "audio" attrs (return())
  where attrs = constDyn $ M.fromList $ zip ["id","controls"] ["userAudio","controls"]

  



--createMediaNode'::MonadWidget t m => m (IO WebAudioNode)
--createMediaNode' = do
  --let attrs = FileInputConfig $ constDyn $ M.singleton "accept" "audio/*"
  --input <- fileInput attrs
  --fileUrlEv <-fileToURL $ fmap (!!0) $ updated $ _fileInput_value input
  --audioSrc <- holdDyn "" fileUrlEv
  --audioAttrs <- mapDyn (M.fromList . zip ["src","class"] . (:["audioElement"])) audioSrc
--  (element,_) <- elDynAttr' "audio" audioAttrs (return ())
--  let elementJSVal = G.unElement $ _el_element element
--  x<-liftIO (F.createMediaNode elementJSVal)
--  return (WebAudioNode (MediaNode "id") x)

mediaElement::MonadWidget t m => String -> m Source
mediaElement audioId = do
  let attrs = FileInputConfig $ constDyn $ M.singleton "accept" "audio/*"
  input <- fileInput attrs
  fileUrlEv <-fileToURL $ fmap (!!0) $ updated $ _fileInput_value input
  audioSrc <- holdDyn "" fileUrlEv
  audioAttrs <- mapDyn (M.fromList . zip ["src","class","id"] . (:["audioElement",audioId])) audioSrc
  elDynAttr "audio" audioAttrs (return ())
  liftIO $ createMediaNode audioId
  return $ NodeSource (MediaNode audioId) 0  -- @ '0' is temporary, this should be a more meaningful duration derrived perhaps from the soundfile

-- Creates file input button and a play/pause/scrub interface.
-- Returns a Source and Event that fires whenever the soundfile changes
-- (to be used to re-connect the graph when the file switches)
--mediaElement::MonadWidget t m => m (Source,(Event t ()))
--mediaElement = el "div" $ do
--  let attrs = FileInputConfig (constDyn $ M.fromList $ zip ["id","accept"] ["soundFileInput","audio/*"])
--  file <- fileInput attrs
--  let fileChange = (()<$) $ updated $ _fileInput_value file
--  audioElement
--  performEvent $ fmap liftIO $ fmap (\_ -> createMediaNode) fileChange   -- loads sound player into audio tag everytime the file changes
--  return (NodeSource (MediaNode "@change") 0,fileChange)


-- Connects nodes to eachother and last node to destination
connectGraphOnEv :: MonadWidget t m => Event t Sound -> m ()
connectGraphOnEv sound = do 
  performEvent $ fmap liftIO $ fmap (\x->do 
    g <- createGraph x
    connectGraphToDest g
    ) sound
  return ()




createAdditiveNode:: [Node] -> IO WebAudioNode
createAdditiveNode xs = do
  nodes <- sequence $ fmap createNode xs -- IO [WebAudioNode]
  g <- F.createGain
  F.setGain 0 g
  sequence (fmap startNode nodes)
  mapM (maybe (return ()) ((flip F.connect) g) . getJSVal) nodes
  return (WebAudioNode (AdditiveNode xs) g) -- returning the gain node's 


createNode:: Node -> IO WebAudioNode
createNode (FilterNode x) = createBiquadFilter x
createNode (GainNode d) = createGain d
createNode (Destination) = error "cannot create destination node"
createNode (AdditiveNode xs) = createAdditiveNode xs
createNode (OscillatorNode x) = createOscillator x
createNode (BufferNode x) = createBufferNode x
createNode (MediaNode s) = createMediaNode s



createMediaNode:: String -> IO WebAudioNode
createMediaNode s = F.createMediaNode (toJSString s) >>= return . (WebAudioNode (MediaNode s))



-- Experimenting://///////


-- returns Event with file's url as a string
fileToURL :: (MonadWidget t m) => Event t G.File -> m (Event t String)
fileToURL file = do
  fileReader <- liftIO newFileReader
  performEvent_ (fmap (\f -> readAsDataURL fileReader (Just f)) file)
  liftM (fmapMaybe id) $ wrapDomEvent fileReader (`on` load) . liftIO $ do
      v <- getResult fileReader
      fromJSVal v

--createMediaNode'::MonadWidget t m => m (IO WebAudioNode)
--createMediaNode' = do
--  let attrs = FileInputConfig $ constDyn $ M.singleton "accept" "audio/*"
--  input <- fileInput attrs
--  fileUrlEv <-fileToURL $ fmap (!!0) $ updated $ _fileInput_value input
--  audioSrc <- holdDyn "" fileUrlEv
--  audioAttrs <- mapDyn (M.fromList . zip ["src","class"] . (:["audioElement"])) audioSrc
--  (element,_) <- elDynAttr' "audio" audioAttrs (return ())
--  let elementJSVal = G.unElement $ _el_element element
--  x<-liftIO (F.createMediaNode elementJSVal)
--  return (WebAudioNode (MediaNode "id") x)


--  return (WebAudioNode MediaNode x)



--data WebAudioNode = WebAudioNode Node JSVal | NullAudioNode

--class WebAudio a where
--  createGraph :: a -> IO WebAudioGraph


--instance WebAudio Source where
--  createGraph (MediaSource) = ??? cannot be createMediaNode???

---- F - refers to FFI
---- where first argument is JSVal of the <audio> element
---- creates the WAAPI audio node and returns JSVal reference to it
--F.createMediaNode :: JSVal -> IO JSVal

---- (not FFI)
---- Calls F.createMediaNode to get a JSVal
---- returns WebAudioNode constructed with the JSVal gotten from F.createMediaNode
--createMediaNode::MonadWidget t m => m (IO WebAudioNode)


---- the problem: running 'createGraph' on a WebAudioNode containing a 
---- media element won't work because createGraph for a Media element would 
---- have to have a type 'tainted' by the MonadWidget monad (incompatible with
---- the signature for createGraph::a->IO WebAudioGraph)



renderAudioWaveform:: G.HTMLCanvasElement -> G.HTMLCanvasElement -> IO()
renderAudioWaveform l r= do 
  let l' = G.unHTMLCanvasElement l
  let r' = G.unHTMLCanvasElement  r
  F.renderAudioWaveform l' r'



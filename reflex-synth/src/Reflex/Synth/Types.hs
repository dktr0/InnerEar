module Reflex.Synth.Types (
  module Reflex.Synth.NodeSpec,
  module Reflex.Synth.WebAudioNode,
  module Reflex.Synth.WebAudioGraph,
  module Reflex.Synth.Sound,
  module Reflex.Synth.WebAudio,
  module Reflex.Synth.Types
  ) where

import Reflex.Synth.NodeSpec
import Reflex.Synth.WebAudioNode
import Reflex.Synth.WebAudioGraph
import Reflex.Synth.Sound
import Reflex.Synth.WebAudio
import qualified Reflex.Synth.Foreign as F

import qualified Data.Map as M
import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import GHCJS.DOM.JSFFI.Generated.HTMLElement
--import GHCJS.DOM.JSFFI.Generated.File (getName)
import GHCJS.DOM.File (getName)
import GHCJS.DOM.FileReader (newFileReader,getResult, readAsDataURL,load)
import GHCJS.DOM.EventM
import GHCJS.DOM.Types(toJSString,HTMLCanvasElement,unHTMLCanvasElement)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal(fromJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Prim (toJSArray)


createAudioContext :: IO ()
createAudioContext = F.createAudioContext

startSilentNode:: IO ()
startSilentNode = F.startSilentNode


performSound:: MonadWidget t m => Event t Sound -> m ()
performSound event = do
  let n = fmap (\e-> do
                      t <- getT e
                      graph <- createGraph e   -- WA''
                      startGraph graph
                      disconnectGraphAtTimeMaybe graph  t
                      ) event          -- Event t (IO ())
  performEvent_ $ fmap liftIO n
--
-- performSound :: MonadWidget t m => Event t Sound -> m ()
-- performSound event = do
--   let n = fmap someName event
--   performEvent_ $ fmap liftIO n
--
-- someName :: Sound -> IO ()
--
-- someName (TwoNoteSound n1 t n2) = do
--   someName n1
--   delayedSomeName t n2
--
-- someName e = do
--   let t = getT e
--   graph <- createGraph e   -- WebAudioNode''
--   startGraph graph
--   disconnectGraphAtTimeMaybe graph  t
--
-- delayedSomeName :: Double -> Sound -> IO ()
-- delayedSomeName t e = do
--   let t2 = getT e
--   graph <- createGraph e
--   delayedStartGraph t graph
--   disconnectGraphAtTimeMaybe graph (t+t2)

audioElement::MonadWidget t m => m ()
audioElement = elDynAttr "audio" attrs (return())
  where attrs = constDyn $ M.fromList $ zip ["id","controls"] ["userAudio","controls"]


bufferInput :: MonadWidget t m => String -> m (Event t ())
bufferInput s = elClass "div" "bufferInput" $ do
  let attrs = FileInputConfig $ constDyn $ M.fromList $ zip ["accept","id"] ["audio/*",s]
  input <- fileInput attrs
  --let element = _fileInput_element input
  --ev <- liftM (() <$) $ wrapDomEvent (onEventName Load) element
  let ev = (() <$) $ updated $ _fileInput_value input
  performEvent_ $ fmap (liftIO . const (F.loadBuffer $ toJSString s)) ev
  return ev



loadAndDrawBuffer :: String -> HTMLCanvasElement -> IO()
loadAndDrawBuffer inputId canvas = do
  let el' = unHTMLCanvasElement canvas
  F.loadAndDrawBuffer (toJSString inputId) el'

  -- Note: - for LoadedFile BufferNode sources, the first argument to LoadedFile (a string)
  --            should correspond to the 'id' attribute of the input element.
  --           - perhaps this should be rethought to be less coupled...
drawSource :: Source -> HTMLCanvasElement -> IO ()
drawSource (NodeSource (BufferNode (LoadedFile identifier (PlaybackParam _ _ _))) _) canvas = loadAndDrawBuffer identifier canvas
drawSource (NodeSource (BufferNode (File s)) _) canvas = F.drawFile (toJSString s) canvas -- @should probably use less javascript here...
drawSource (NodeSource (OscillatorNode (Oscillator Sine _ _) )_) canvas = F.drawSineWave canvas
drawSource _ _ = return ()



-- drawStartEnd :: PlaybackParam -> HTMLCanvasElement -> IO ()
-- drawStartEnd (PlaybackParam s e _) c = F.drawStartEnd (pToJSVal s) (pToJSVal e) (unHTMLCanvasElement c)


createAudioElement::MonadWidget t m => String -> Dynamic t (M.Map String String) -> m (String)
createAudioElement s m = elDynAttr "audio" m (return s)



-- creates graph and connects it to dest for initial source.
-- when event fires, disconnects source from everything and connects source to
-- the Node contained in the event. connects the node to the dest.
-- useful for swapping 'effects' on MediaNodes
holdAndConnectSound:: MonadWidget t m => Source -> Event t NodeSpec -> m ()
holdAndConnectSound s ev = do
  g <- liftIO (createGraph s)
  liftIO (connectGraphToDest g)
  performEvent $ fmap liftIO $ fmap (\n -> do
    let gLast = getLastNode g
    disconnectAll gLast
    newNode <- createNode n
    newGraph <- connect gLast newNode
    connectGraphToDest newGraph
    ) ev
  return ()

-- Connects nodes to eachother and last node to destination
connectGraphOnEv :: MonadWidget t m => Event t Sound -> m ()
connectGraphOnEv sound = do
  performEvent $ fmap liftIO $ fmap (\x->do
    g <- createGraph x
    connectGraphToDest g
    ) sound
  return ()

drawSineWave:: HTMLCanvasElement  -> IO ()
drawSineWave el  = F.drawSineWave (el)

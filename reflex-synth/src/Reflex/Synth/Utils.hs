module Reflex.Synth.Utils where

import GHCJS.DOM.EventM
import qualified GHCJS.DOM.FileReader as FR
import GHCJS.DOM.File
import GHCJS.Marshal.Pure(pFromJSVal, toJSVal)
import GHCJS.Prim(fromJSString)
import Control.Monad.IO.Class(MonadIO(..))
import Reflex.Dom.Widget.Basic
import Reflex.Class
import Reflex.Synth.Node(globalAudioContext)
import Reflex.Synth.AudioRoutingGraph(AudioBuffer, ArrayBuffer, WebAudioContext)

loadAudioBufferFromFile :: (MonadIO m, Reflex t) => File -> m (Event t (Either String AudioBuffer))
loadAudioBufferFromFile file = do
  reader <- FR.newFileReader
  FR.readAsArrayBuffer reader $ Just file

  loadedEv <- wrapDomEvent reader (`on` FR.load) $ do
    jsResult <- FR.getResult reader
    return $ pFromJSVal jsResult
  decodedEv <- fmap (decodeArrayBuffer globalAudioContext) loadedEv

  errorEv <- wrapDomEvent reader (`on` FR.error) $ do
    maybeErr <- FR.getError reader -- TODO this doesn't work so great
    msg <- getMessage maybeErr
    return $ Left $ fromJSString msg
  abortedEv <- wrapDomEvent reader (`on` FR.abortEvent) $ return $ Left "Aborted"

  -- No worries with leftmost, the events are mutually exclusive
  return $ leftmost [decodedEv, errorEv, abortedEv]

decodeArrayBuffer :: (MonadIO m, Reflex t) => WebAudioContext -> ArrayBuffer -> m (Event t (Either String AudioBuffer))
decodeArrayBuffer ctx buffer = do
  -- TODO needs to be implemented
  undefined
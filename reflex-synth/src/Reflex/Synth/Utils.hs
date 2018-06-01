module Reflex.Synth.Utils (
	loadAudioBufferFromFile,
	decodeArrayBuffer,
) where

import GHCJS.DOM.EventM
import qualified GHCJS.DOM.FileReader as FR
import GHCJS.DOM.File
import GHCJS.DOM.FileError
import GHCJS.DOM.Types(Nullable, maybeToNullable)
import GHCJS.Marshal.Pure(pFromJSVal)
import Data.JSString(JSString, unpack)
import GHCJS.Prim(fromJSString)
import Control.Monad.IO.Class(MonadIO(..))
import Reflex.Dom.Widget.Basic
import Reflex.Dom.Class(MonadWidget, performEvent)
import Reflex.Class
import Reflex.Synth.Node(globalAudioContext)
import Reflex.Synth.AudioRoutingGraph(AudioBuffer, ArrayBuffer, WebAudioContext)

-- See https://github.com/reflex-frp/reflex-examples/blob/b7319cb824c93114b019abc66611be29e87f923a/fileinput/src/Main.hs#L48
-- for a nice example of reading a file input.

loadAudioBufferFromFile :: MonadWidget t m => File -> m (Event t (Either String AudioBuffer))
loadAudioBufferFromFile file = do
  reader <- FR.newFileReader
  FR.readAsArrayBuffer reader $ Just file

  -- The loadedEv fires when the file is finished loading and the decoding
  -- process **begins**. This is an important distinction. As such,
  -- loadedEv :: Event t (Event t (Either String AudioBuffer))
  loadedEv <- wrapDomEvent reader (`on` FR.load) $ liftIO $ do
    buf <- pFromJSVal <$> FR.getResult reader
    ctx <- globalAudioContext
    decodeArrayBuffer ctx buf

  -- The decodedEv fires when the buffer has loaded and **finishes** decoing.
  -- decodedEv :: Event t (Either String AudioBuffer)
  decodedEv <- joinEv loadedEv

  -- The errorEv and abortedEv are exceptional cases for gracefully propagating
  -- error information to whoever is waiting on the data to be decoded.
  errorEv <- wrapDomEvent reader (`on` FR.error) $ liftIO $ do
    maybeErr <- FR.getError reader
    msg <- js_getErrorMessage $ maybeToNullable maybeErr
    return $ Left $ unpack msg
  abortedEv <- wrapDomEvent reader (`on` FR.abortEvent) $ return $ Left "Aborted"

  -- No worries with leftmost, the events are mutually exclusive
  return $ leftmost [decodedEv, errorEv, abortedEv]

decodeArrayBuffer :: (MonadIO m, Reflex t) => WebAudioContext -> ArrayBuffer -> m (Event t (Either String AudioBuffer))
decodeArrayBuffer ctx buffer = do
  error "Not yet implemented! decodeArrayBuffer"

foreign import javascript safe
  "$1 != null ? $1['message'] : ''"
  js_getErrorMessage :: Nullable FileError -> IO JSString

-- foreign import javascript safe
--   "loadAndDecode($1, $2);"
--   js_loadAndDecode :: File -> Callback (JSVal -> IO ()) -> 

-- THANK YOU! https://lambdasistemi.net/public/snippets/DynamicListH/
-- Found this gem in the above sample under a "reflex missings" header which
-- couldn't be more accurate. 

-- |joinEv creates an event that fires when the inner event fires.
joinEv :: (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
joinEv = fmap switch . hold never
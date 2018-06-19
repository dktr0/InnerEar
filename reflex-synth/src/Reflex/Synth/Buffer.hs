module Reflex.Synth.Buffer (
  Buffer(..),
  BufferStatus(..),
  mapToBuffer,
  buffer,
  loadGlobalResources
) where

import Control.Monad.IO.Class
import Control.Monad (forM,liftM)
import Data.Map
import Data.JSString(JSString, unpack, pack)

import GHCJS.DOM.File
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback

import JavaScript.Cast

import Reflex
import Reflex.Dom.Class
import Reflex.Synth.AudioRoutingGraph

newtype Buffer = Buffer JSVal

instance Show Buffer where show _ = "Buffer"

instance PToJSVal Buffer where pToJSVal (Buffer val) = val

instance ToJSVal Buffer where toJSVal (Buffer v) = return v
-- instance ToJSVal [Buffer] where toJSVal bufs= toJSValListOf

  --  toJSValListOf :: [a] -> IO JSVal

data BufferStatus
  = BufferUnderspecified
  | BufferLoading
  | BufferError String
  | BufferLoaded AudioBuffer

isBufferLoaded::BufferStatus -> Bool
isBufferLoaded (BufferLoaded _) = True
isBufferLoaded _ = False

bufferStatus :: Buffer -> IO (Maybe BufferStatus)
bufferStatus buffer = do
  rawStatus <- js_getBufferStatus buffer
  case unpack rawStatus of
    "loading" -> return $ Just BufferLoading
    "decoding" -> return $ Just BufferLoading
    "error" -> do
      err <- js_getBufferError buffer
      return $ Just $ BufferError $ unpack err
    "decoded" -> do
      audioBuffer <- js_getAudioBuffer buffer
      return $ Just $ BufferLoaded audioBuffer
    otherwise -> return Nothing

--
--
--   asyncCallback1 :: ([Buffers] -> IO ())            -- ^ the function that the callback calls
-- -> IO (Callback ([Buffers]-> IO ())) -- ^ the calback
--
-- cb <- asyncCallback1 $ \bufs -> evTrigger (fmap Buffer bufs)
-- evTrigger::[Buffer] -> IO

-- performEventAsync :: forall t m a. MonadWidget t m => Event t ((a -> IO ()) -> WidgetHost m ()) -> m (Event t a)
-- --
-- performEventAsync :: forall t m a. MonadWidget t m => Event t (([buffers] -> IO ()) -> WidgetHost m ()) -> m (Event t a)
--
--     asyncCallback1 :: (JSVal -> IO ())            -- ^ the function that the callback calls
--   -> IO (Callback (JSVal -> IO ())) -- ^ the calback

 -- fromJSValListOf :: JSVal -> IO (Maybe [a])

loadGlobalResources :: MonadWidget t m => m ( Event t  (Map String AudioBuffer))
loadGlobalResources = do
  let sources = ["pinknoise.wav", "whitenoise.wav"]
  pb <- getPostBuild
  -- loadResources loadRequestEv
  bufferEv <- loadResources $ fmap (const sources) pb -- Ev (Map String Buffer)
  let r = fmap (fmap js_getAudioBuffer) bufferEv -- Ev (Map String (IO AudioBuffer))
  let r' = fmap (liftIO . sequence) r  -- ev (io (Map String AudioBuffer))
  performEvent r'

loadResources:: MonadWidget t m => Event t [String] -> m (Event t (Map String Buffer))
loadResources ev = do
  bufferEv <- performEvent $ ffor ev $ \fileNames -> liftIO $ do                     -- Ev t [(String,Buffer)]
    ctx <- js_setupGlobalAudioContext -- really just get global ac
    forM fileNames $ \s -> do
      buf <- js_createBufferFromURL (pack s) ctx
      return (s,buf)
  stateChangeEv <- performEventAsync $ ffor bufferEv $ \buffers evTrigger -> liftIO $ do  -- Event t [Buffer]
    -- cbs <- forM buffers $ \(s,buf) -> asyncCallback1 $ \buf -> evTrigger (Buffer buf)  -- IO [cb]
    cb <- asyncCallback1 $ \bufs -> do
      maybeBufs <- fromJSValListOf bufs -- IO (Maybe [Buffer])
      evTrigger (maybe [] (fmap Buffer) maybeBufs)
    func (fmap snd buffers) cb
    releaseCallback cb
  statusEv <- performEvent $ ffor stateChangeEv $ \buffers -> do   -- Event t [Maybe BufferStatus]
    liftIO $ sequence $ fmap bufferStatus buffers -- IO [(Maybe BufferStatus)] then lifted
  let areLoaded = fmap (and . fmap (maybe False (isBufferLoaded))) statusEv
  dynBuffers <- holdDyn Data.Map.empty $ fmap fromList bufferEv
  return $ tagDyn dynBuffers $ ffilter id areLoaded


mapToBuffer :: MonadWidget t m => Event t File -> m (Event t Buffer, Event t BufferStatus)
mapToBuffer fileEv = do
  -- bufferEv :: Event t Buffer - a buffer ready to start loading it's file
  bufferEv <- performEvent $ ffor fileEv $ \file -> liftIO $ do
    ctx <- js_setupGlobalAudioContext
    js_createBuffer file ctx

  -- stateChangeEv :: Event t Buffer - tiggered on a status change
  stateChangeEv <- performEventAsync $ ffor bufferEv $ \buffer evTrigger -> liftIO $ do
    cb <- asyncCallback1 $ \buf -> evTrigger (Buffer buf)
    js_startLoadingAndDecoding buffer cb
    releaseCallback cb

  -- statusEv' :: Event t BufferStatus - triggered on **relevant** status changes, hence the fmapMaybe
  statusEv <- performEvent $ fmap (liftIO . bufferStatus) stateChangeEv
  let statusEv' = fmapMaybe id statusEv

  return (stateChangeEv, statusEv')

-- | buffer creates a smart buffer for asynchronous loading of the most recent `Just` file fired
-- from the `Event t (Maybe File)`. Until the first occurance of the event, the buffer is `Nothing`.
-- The returned buffer status monitors the current state of the buffer.
buffer :: MonadWidget t m => Event t (Maybe File) -> m (Dynamic t (Maybe Buffer), Dynamic t BufferStatus)
buffer maybeFileEv = do
  (bufferEv, statusEv) <- mapToBuffer (fmapMaybe id maybeFileEv)
  dynBuffer <- holdDyn Nothing $ fmap Just bufferEv
  dynStatus <- holdDyn BufferUnderspecified statusEv
  return (dynBuffer, dynStatus)

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBuffer :: File -> WebAudioContext -> IO Buffer

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBufferFromURL :: JSString -> WebAudioContext -> IO Buffer


foreign import javascript safe
  "$1.startLoadingAndDecoding($2);"
  js_startLoadingAndDecoding :: Buffer -> Callback (JSVal -> IO ()) -> IO ()
  --                                                 ^ actually Buffer
foreign import javascript safe
  "startLoadingAndDecodingMultiple($1, $2)"
  js_startLoadingAndDecodingMultiple:: JSVal -> Callback (JSVal -> IO ()) -> IO ()

func:: [Buffer] -> Callback (JSVal -> IO ()) -> IO ()
func b cb  = do
  listJSVal <- toJSValListOf b
  js_startLoadingAndDecodingMultiple listJSVal cb

foreign import javascript safe
  "$1.status"
  js_getBufferStatus :: Buffer -> IO JSString

foreign import javascript safe
  "$1.error"
  js_getBufferError :: Buffer -> IO JSString

foreign import javascript safe
  "$1.buffer"
  js_getAudioBuffer :: Buffer -> IO AudioBuffer

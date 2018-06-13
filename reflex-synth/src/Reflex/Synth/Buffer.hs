module Reflex.Synth.Buffer (
  Buffer,
  mapToBuffer
) where

import Control.Monad.IO.Class

import Data.JSString(JSString, unpack)

import GHCJS.DOM.File
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback

import JavaScript.Cast

import Reflex
import Reflex.Dom.Class
import Reflex.Synth.AudioRoutingGraph
  
newtype Buffer = Buffer JSVal 

instance Show Buffer where show _ = "Buffer"

instance PToJSVal Buffer where pToJSVal (Buffer val) = val

data BufferStatus 
  = BufferNotLoaded 
  | BufferError String
  | BufferLoaded AudioBuffer

bufferStatus :: Buffer -> IO (Maybe BufferStatus)
bufferStatus buffer = do
  rawStatus <- js_getBufferStatus buffer
  case unpack rawStatus of
    "created" -> return $ Just BufferNotLoaded
    "error" -> do
      err <- js_getBufferError buffer
      return $ Just $ BufferError $ unpack err
    "decoded" -> do
      audioBuffer <- js_getAudioBuffer buffer
      return $ Just $ BufferLoaded audioBuffer
    _ -> return Nothing

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

buffer :: MonadWidget t m => Event t (Maybe File) -> m (Dynamic t (Maybe (Buffer, BufferStatus)))
buffer fileEv = do
  error "Not yet implemented"
  -- (bufferEv, statusEv) <- mapToBuffer fileEv
  -- holdDyn

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBuffer :: File -> WebAudioContext -> IO Buffer

foreign import javascript safe
  "$1.startLoadingAndDecoding($2);"
  js_startLoadingAndDecoding :: Buffer -> Callback (JSVal -> IO ()) -> IO ()
  --                                                 ^ actually Buffer

foreign import javascript safe
  "$1.status" 
  js_getBufferStatus :: Buffer -> IO JSString

foreign import javascript safe
  "$1.error"
  js_getBufferError :: Buffer -> IO JSString

foreign import javascript safe
  "$1.buffer"
  js_getAudioBuffer :: Buffer -> IO AudioBuffer

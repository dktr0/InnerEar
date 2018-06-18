module Reflex.Synth.Buffer (
  Buffer(..),
  BufferStatus(..),
  bufferStatus,
  createBuffer,
  startLoadingAndDecodingWithCallback,
) where

import Data.JSString(unpack)

import GHCJS.DOM.File
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback

import Reflex.Synth.AudioRoutingGraph
  
newtype Buffer = Buffer JSVal 

instance Show Buffer where show _ = "Buffer"

instance PToJSVal Buffer where pToJSVal (Buffer val) = val

data BufferStatus 
  = BufferUnderspecified
  | BufferLoading
  | BufferError String
  | BufferLoaded AudioBuffer

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
    _ -> return Nothing

createBuffer :: File -> IO Buffer
createBuffer file = do
  ctx <- js_setupGlobalAudioContext
  js_createBuffer file ctx

startLoadingAndDecodingWithCallback :: Buffer -> (Buffer -> IO ()) -> IO ()
startLoadingAndDecodingWithCallback buffer evTrigger = do
  cb <- asyncCallback1 $ \buf -> evTrigger (Buffer buf)
  js_startLoadingAndDecoding buffer cb
  releaseCallback cb


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

{-# LANGUAGE JavaScriptFFI, DeriveDataTypeable #-}
module InnerEar.WebSocket where

import Reflex
import qualified Reflex.Dom as R
import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock
import Data.Time.Calendar (Day(ModifiedJulianDay))
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure
import Control.Monad.IO.Class (liftIO)
import Data.Text

import InnerEar.Types.Request
import InnerEar.Types.Response

data WebSocket = WebSocket (Maybe T.JSVal)

webSocket :: IO WebSocket
webSocket = webSocket_ >>= return . WebSocket . Just

send :: WebSocket -> Request -> IO ()
send (WebSocket Nothing) _ = return ()
-- send (WebSocket (Just ws)) x = send_ ws $ Prim.toJSString $ encode $ toJSON x -- was just encode
send (WebSocket (Just ws)) x = send_ ws $ Prim.toJSString $ encodeJSON x
--send (WebSocket (Just ws)) x = do
--  let a = toJSON x :: JSValue
--  let b = encode a :: String
--  let c = pToJSVal b :: T.JSVal
--  send_ ws c

reflexWebSocket :: R.MonadWidget t m => Event t Request -> m (Event t [Response],Dynamic t String)
reflexWebSocket toSend = do
  postBuild <- R.getPostBuild
  newWs <- R.performEvent $ fmap (liftIO . (const webSocket)) postBuild
  ws <- holdDyn (WebSocket Nothing) newWs
  let wsAndToSend = attachDyn ws toSend
  R.performEvent_ $ fmap (liftIO . (\(y,z) -> send y z)) wsAndToSend
  let aLongTimeAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
  ticks <- R.tickLossy (0.05::NominalDiffTime) aLongTimeAgo
  let wsTick = tagDyn ws ticks
  responses <- R.performEvent $ fmap (liftIO . getResponses) wsTick
  let responses' = fmapMaybe id $ fmap (either (const Nothing) (Just)) responses
  status <- R.performEvent $ fmap (liftIO . getStatus) wsTick
  status' <- holdDyn "" status
  return (responses',status')


getResponses :: WebSocket -> IO (Either String [Response])
getResponses (WebSocket (Just ws)) = do
  rs <- getResponses_ ws :: IO T.JSVal
  let a = Prim.fromJSString rs :: String
  let c = decodeResponses a
  -- let b = JSString (toJSString a) :: JSValue
  -- let c = fromJSON b :: Result [Response]
  return $ f c
  where f (Ok xs) = Right xs
        f (Error x) = Left $ "error trying to parse this in getResponses: " ++ x
getResponses (WebSocket Nothing) = return (Right [])

getStatus :: WebSocket -> IO String
getStatus (WebSocket (Just ws)) = Prim.fromJSString <$> getStatus_ ws
getStatus (WebSocket Nothing) = return ""

getHostName :: IO String
getHostName = Prim.fromJSString <$> getHostName_

getPort :: IO String
getPort = Prim.fromJSString <$> getPort_


-- Javascript FFI below this line:

foreign import javascript unsafe
  "$r = new InnerEarWebSocket(4468)"
  webSocket_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.send($2)"
  send_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrl_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName_ :: IO T.JSVal

foreign import javascript unsafe
  "$r = location.port"
  getPort_ :: IO T.JSVal

foreign import javascript unsafe
  "$r = $1.getResponses()"
  getResponses_ :: T.JSVal -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.status"
  getStatus_ :: T.JSVal -> IO T.JSVal

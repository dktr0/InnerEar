{-# LANGUAGE JavaScriptFFI #-}
module InnerEar.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import Data.Time.Clock
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure

data WebSocket = WebSocket (Maybe T.JSVal)

webSocket :: IO WebSocket
webSocket = webSocket_ >>= return . WebSocket . Just

send :: JSON a => WebSocket -> a -> IO ()
send (WebSocket Nothing) _ = return ()
send (WebSocket (Just ws)) x = send_ ws $ Prim.toJSString $ encode x

reflexWebSocket :: (MonadWidget t m, JSON a, JSON b)
  => Event t a -> m (Event t [b],Dynamic t String)
reflexWebSocket toSend = do
  postBuild <- getPostBuild
  newWs <- performEvent $ fmap (liftIO . (const webSocket)) postBuild
  ws <- holdDyn (WebSocket Nothing) newWs
  wsAndToSend <- attachDyn wsAsDyn toSend
  performEvent_ $ fmap (liftIO . (\(y,z) -> send y z)) wsAndToSend
  let aLongTimeAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
  ticks <- tickLossy (0.05::NominalDiffTime) aLongTimeAgo
  wsTick <- tagDyn wsAsDyn ticks
  responses <- performEvent $ fmap (liftIO . getResponses) wsTick
  let responses' = fmapMaybe id $ fmap (either (const Nothing) (Just)) responses
  status <- performEvent $ fmap (liftIO . getStatus) wsTick
  status' <- holdDyn "" status
  return (responses',status')


getResponses :: JSON a => WebSocket -> IO (Either String [a])
getResponses (WebSocket (Just ws)) = (f . decode . Prim.fromJSString) <$> getResponses_ ws
  where f (Ok xs) = Right xs
        f (Error x) = Left $ "error trying to parse this in getResponses: " ++ x
getResponses (WebSocket Nothing) = return ()

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

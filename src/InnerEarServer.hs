{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import qualified Network.WebSockets as WS
import qualified Network.Wai as WS
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, ssIndices)
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Types (unsafeToPiece)
import Text.JSON
import Text.JSON.Generic
import Data.Map
import Data.Text (Text)
import Data.List ((\\))
import Data.Maybe (fromMaybe,isJust,fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Data.Either
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception (try,catch,SomeException)
import Data.Time.Clock

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Server
import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Data
import InnerEar.Types.User

import qualified InnerEar.Database.SQLite as DB
import qualified InnerEar.Database.Users as DB
import qualified InnerEar.Database.Events as DB


main = do
  db <- DB.openDatabase
  s <- newMVar $ newServer db
  mainWithDatabase s `catch` (closeDatabaseOnException s)

closeDatabaseOnException :: MVar Server -> SomeException -> IO ()
closeDatabaseOnException s e = do
  s' <- takeMVar s
  putStrLn $ "quitting and closing database due to unhandled exception (" ++ (show e) ++ ")..."
  DB.closeDatabase $ database s'
  putStrLn "database connection closed."

mainWithDatabase :: MVar Server -> IO ()
mainWithDatabase s = do
  putStrLn "Inner Ear server (listening on port 8000)"
  let settings = (defaultWebAppSettings "InnerEarClient.jsexe") { ssIndices = [unsafeToPiece "index.html"] }
  run 8000 $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp s) (staticApp settings)

webSocketsApp :: MVar Server -> WS.ServerApp -- = PendingConnection -> IO ()
webSocketsApp s ws = do
  putStrLn "received new connection"
  ws' <- WS.acceptRequest ws
  ss <- takeMVar s
  let (i,ss') = addConnection ws' ss
  putMVar s ss'
  WS.forkPingThread ws' 30
  processLoop ws' s i

processLoop :: WS.Connection -> MVar Server -> ConnectionIndex -> IO ()
processLoop ws s i = do
  m <- try $ WS.receiveData ws
  case m of
    Right x -> do
      let x' = decode (T.unpack x) :: Result JSString
      case x' of
        Ok x'' -> do
          processResult s i $ (decodeRequest . fromJSString) x''
          processLoop ws s i
        Error x'' -> do
          putStrLn $ "Error (processLoop): " ++ x''
          processLoop ws s i
    Left WS.ConnectionClosed -> close s i "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s i "connection closed by request from peer"
    Left (WS.ParseException e) -> do
      putStrLn ("parse exception: " ++ e)
      processLoop ws s i

close :: MVar Server -> ConnectionIndex -> String -> IO ()
close s i msg = do
  putStrLn $ "closing connection: " ++ msg
  s' <- takeMVar s
  sessionEnd i s'
  putMVar s s'
  updateServer s $ deleteConnection i
  return ()

processResult :: MVar Server -> ConnectionIndex -> Result Request -> IO ()
processResult _ i (Error x) = putStrLn ("Error (processResult): " ++ x)
processResult s i (Ok x) = processRequest s i x

processRequest :: MVar Server -> ConnectionIndex -> Request -> IO ()
processRequest s i (CreateUser h p) = putStrLn "warning: ignoring request from client to CreateUser (that functionality is disactivated in this build)"
processRequest s i (Authenticate h p) = withServer s $ authenticate i h p
processRequest s i Deauthenticate = withServer s $ deauthenticate i
processRequest s i (PostPoint r) = withServer s $ postPoint i r


authenticate :: ConnectionIndex -> Handle -> Password -> Server -> IO Server
authenticate i h p s = do
  u <- DB.findUser (database s) h
  case u of
    (Just u') -> do
      let p' = password u'
      let r = role u'
      if p == p' then do
        putStrLn $ "authenticated as user " ++ h
        now <- getCurrentTime
        DB.postEvent (database s) $ Record h $ Point (Right SessionStart) now
        respond s i $ Authenticated h r
        return $ authenticateConnection i h s
      else do
        putStrLn $ "failure to authenticate as user " ++ h
        now <- getCurrentTime
        DB.postEvent (database s) $ Record h $ Point (Right AuthenticationFailure) now
        respond s i $ NotAuthenticated
        return $ deauthenticateConnection i s
    Nothing -> do
      putStrLn $ "failure to authenticate as non-existent user " ++ h
      now <- getCurrentTime
      DB.postEvent (database s) $ Record h $ Point (Right AuthenticationFailure) now
      respond s i $ NotAuthenticated
      return $ deauthenticateConnection i s

deauthenticate :: ConnectionIndex -> Server -> IO Server
deauthenticate i s = do
  putStrLn $ "deauthenticating connection " ++ (show i)
  s' <- sessionEnd i s
  respond s' i $ NotAuthenticated
  return $ deauthenticateConnection i s'

sessionEnd :: ConnectionIndex -> Server -> IO Server
sessionEnd i s = do
  now <- getCurrentTime
  let h = snd $ (connections s) ! i
  when (isJust h) $ DB.postEvent (database s) $ Record (fromJust h) $ Point (Right SessionEnd) now
  return s

postPoint :: ConnectionIndex -> Point -> Server -> IO Server
postPoint i p s = do
  let h = snd ((connections s) ! i)
  if isJust h
    then do
      let r = Record (fromJust h) p
      putStrLn $ "posting record: " ++ (show r)
      DB.postEvent (database s) r
      return s
    else do
      putStrLn $ "warning: received post record attempt from non-or-differently-authenticated connection"
      return s

withServer :: MVar Server -> (Server -> IO Server) -> IO ()
withServer s f = takeMVar s >>= f >>= putMVar s

respond :: Server -> ConnectionIndex -> Response -> IO ()
respond s i x = WS.sendTextData c t
  where
    c = fst $ (Map.! i) $ (connections s)
    t = T.pack $ encode $ toJSON x

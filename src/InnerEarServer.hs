module Main where

import qualified Network.WebSockets as WS
import Text.JSON
import Data.Map
import Data.Text (Text)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception (try)

import InnerEar.Types.Request
import InnerEar.Types.Response
import InnerEar.Types.Server
import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.Record

main = do
  putStrLn "Inner Ear server (listening on port 4468)"
  let ourServer = newServer
  server <- newMVar ourServer
  WS.runServer "0.0.0.0" 8001 $ connectionHandler server

connectionHandler :: MVar Server -> WS.PendingConnection -> IO ()
connectionHandler s ws = do
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
          processResult s i $ decode (fromJSString x'')
          processLoop ws s i
        Error x'' -> do
          putStrLn $ "Error: " ++ x''
          processLoop ws s i
    Left WS.ConnectionClosed -> close s i "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s i "connection closed by request from peer"
    Left (WS.ParseException e) -> do
      putStrLn ("parse exception: " ++ e)
      processLoop ws s i

close :: MVar Server -> ConnectionIndex -> String -> IO ()
close s i msg = do
  putStrLn $ "closing connection: " ++ msg
  updateServer s $ deleteConnection i
  return ()

processResult :: MVar Server -> ConnectionIndex -> Result Request -> IO ()
processResult _ i (Error x) = putStrLn ("Error: " ++ x)
processResult s i (Ok x) = processRequest s i x

processRequest :: MVar Server -> ConnectionIndex -> Request -> IO ()
processRequest s i (CreateUser h p) = withServer s $ createUser i h p
processRequest s i (Authenticate h p) = withServer s $ authenticate i h p
processRequest s i Deauthenticate = withServer s $ deauthenticate i
processRequest s i (PostRecord r) = withServer s $ postRecord i r

createUser :: ConnectionIndex -> Handle -> Password -> Server -> IO Server
createUser i h p s = if userExists h s
  then do
    respond s i $ NotAuthenticated
    return s
  else do
    respond s i $ Authenticated h
    return $ addUser i h p s

authenticate :: ConnectionIndex -> Handle -> Password -> Server -> IO Server
authenticate i h p s = if userExists h s
  then do
    if p == getPassword h s
      then do
        putStrLn $ "authenticated as user " ++ h
        respond s i $ Authenticated h
        -- placeholder: ...could also dump all records for now here...
        return $ authenticateConnection i h s
      else do
        putStrLn $ "failure to authenticate as user " ++ h
        respond s i $ NotAuthenticated
        return $ deauthenticateConnection i s
  else do
    respond s i $ NotAuthenticated
    return s

deauthenticate :: ConnectionIndex -> Server -> Server
deauthenticate i s = do
  respond s i $ NotAuthenticated
  return $ deauthenticateConnection i s

postRecord :: ConnectionIndex -> Record -> Server -> IO Server
postRecord i r@(Record h p) s = if ok then doIt else dont
  where
    cHandle = snd ((connections s) ! i)
    rHandle = Just h
    ok = cHandle == rHandle
    doIt = do
      putStrLn $ "posting record: " ++ (show r)
      return $ postPoint h p s
    dont = do
      putStrLn $ "unable to post record (not authenticated or authenticated to different handle)" ++ (show r)
      return s

withServer :: MVar Server -> (Server -> IO Server) -> IO ()
withServer s f = takeMVar s >>= f >>= putMVar s

respond :: Server -> ConnectionIndex -> Response -> IO ()
respond s i x = WS.sendTextData c t
  where
    c = fst $ (Map.! i) $ (connections s)
    t = T.pack $ encodeStrict x

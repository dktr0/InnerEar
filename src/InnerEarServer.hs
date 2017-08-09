module Main where

import qualified Network.WebSockets as WS
import Text.JSON
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

processRequest s i (CreateUser h p) = withServer s $ \s' -> do
  if userExists h s'
    then do
      respond s' i $ NotAuthenticated
      return s'
    else do
      respond s' i $ Authenticated h
      return $ createUser i h p s'

processRequest s i (Authenticate h p) = withServer s $ \s' -> do
  if userExists h s'
    then do
      if p == getPassword h s'
        then do
          putStrLn $ "authenticated as user " ++ h
          respond s' i $ Authenticated h
          return $ authenticateUser h s'
        else do
          putStrLn $ "failure to authenticate as user " ++ h
          respond s' i $ NotAuthenticated
          return $ deauthenticateUser h s'
    else do
      respond s' i $ NotAuthenticated
      return s'

processRequest s i (PostRecord r) = putStrLn "placeholder: PostRecord not handled yet"

withServer :: MVar Server -> (Server -> IO Server) -> IO ()
withServer s f = takeMVar s >>= f >>= putMVar s


respond :: Server -> ConnectionIndex -> Response -> IO ()
respond s i x = WS.sendTextData c t
  where
    c = fst $ (Map.! i) $ (connections s)
    t = T.pack $ encodeStrict x

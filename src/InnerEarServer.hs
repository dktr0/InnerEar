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

main = putStrLn "Inner Ear"

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

close :: MVar Server -> ConnectionHandle -> String -> IO ()
close s i msg = do
  putStrLn $ "closing connection: " ++ msg
  updateServer s $ deleteConnection i
  return ()

processResult :: MVar Server -> ConnectionIndex -> Result Request -> IO ()
processResult _ i (Error x) = putStrLn ("Error: " ++ x)
processResult s i (Ok x) = processRequest s i x


processRequest :: MVar Server -> ConnectionIndex -> Request -> IO ()

processRequest s i (CreateUser h p) = do
  s' <- takeMVar s
  s'' <- if handle already exists...
    then do
      -- placeholder: send back fail message
      return s'
    else do
      -- placeholder: send back authenticated message
      return $ createUser h p s'
  putMVar s s''









send :: ServerResponse -> [Client] -> IO ()
send x cs = do
  -- putStrLn $ "send to " ++ (show (length cs))
  mapM_ f cs
  where f c = WS.sendTextData (connection c) $ (T.pack . encodeStrict) x

respond :: MVar Server -> ClientHandle -> ServerResponse -> IO ()
respond s c x = withMVar s $ (send x) . (:[]) . (Map.! c)  . clients

-- respond' is for use when one already has a lock on the server MVar'
respond' :: Server -> ClientHandle -> ServerResponse -> IO ()
respond' s c x = send x $ (:[]) $ (Map.! c) $ clients s

respondAll :: MVar Server -> ServerResponse -> IO ()
respondAll s x = withMVar s $ (send x) . Map.elems . clients

respondAllNoOrigin :: MVar Server -> ClientHandle -> ServerResponse -> IO ()
respondAllNoOrigin s c x = withMVar s $ (send x) . Map.elems . Map.delete c . clients

respondEnsemble :: MVar Server -> String -> ServerResponse -> IO ()
respondEnsemble s e x = withMVar s $ (send x) . Map.elems . ensembleFilter e . clients

respondEnsembleNoOrigin :: MVar Server -> ClientHandle -> String -> ServerResponse -> IO ()
respondEnsembleNoOrigin s c e x = withMVar s $ (send x) . Map.elems . Map.delete c . ensembleFilter e . clients

ensembleFilter :: String -> Map.Map ClientHandle Client -> Map.Map ClientHandle Client
ensembleFilter e = Map.filter $ (==(Just e)) . ensemble

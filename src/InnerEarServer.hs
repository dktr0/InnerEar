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
import Data.Either.Combinators (fromLeft')
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
import InnerEar.Types.ExerciseId
import InnerEar.Types.User

import qualified InnerEar.Database.SQLite as DB
import qualified InnerEar.Database.Users as DB
import qualified InnerEar.Database.Events as DB
import qualified InnerEar.Database.Stores as DB


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
processRequest s i (CreateUser h p) = withServer s $ createUser i h p
processRequest s i (Authenticate h p) = withServer s $ authenticate i h p
processRequest s i Deauthenticate = withServer s $ deauthenticate i
processRequest s i (PostPoint r) = withServer s $ postPoint i r
processRequest s i GetUserList = withServer s $ getUserList i
processRequest s i (GetAllRecords h) = withServer s $ getAllRecords i h
processRequest s i (GetAllExerciseEvents h e) = withServer s $ getAllExerciseEvents i h e

createUser :: ConnectionIndex -> Handle -> Password -> Server -> IO Server
createUser i newHandle p s = do
  let h = getHandle i s
  u <- maybe (return Nothing) (DB.findUser (database s)) h
  let r = maybe Nothing (Just . role) u
  if canSeeUserList r then do -- note: if you can see the user list then you can add new users...
    x <- DB.addUser (database s) $ User { handle = newHandle, password = p, role = NormalUser } 
    if isLeft x then do
      let errorMsg = fromLeft' x
      putStrLn $ "attempt to create user " ++ newHandle ++ " failed because: " ++ errorMsg
      respond s i $ UserNotCreated errorMsg
    else do
      putStrLn $ "createUser " ++ newHandle ++ " succeeded"
      respond s i $ UserCreated 
  else do
    let errorMsg = "user originating request is not authenticated"
    putStrLn $ "attempt to create user " ++ newHandle ++ " failed because: " ++ errorMsg
    respond s i $ UserNotCreated errorMsg
  return s

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
        allStores <- DB.findAllStores (database s) h
        forM allStores $ respond s i . StoreResponse
        respond s i $ AllStoresSent 
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
  let h = snd $ (connections s) ! i -- should this be guarded against lookup failure, using getHandle like in getUserList below?
  when (isJust h) $ DB.postEvent (database s) $ Record (fromJust h) $ Point (Right SessionEnd) now
  return s

postPoint :: ConnectionIndex -> Point -> Server -> IO Server
postPoint i p s = do
  let h = snd ((connections s) ! i) -- should this be guarded against lookup failure, using getHandle like in getUserList below?
  if isJust h
    then do
      possiblySendAllExerciseData i p s
      let r = Record (fromJust h) p
      putStrLn $ "posting record: " ++ (show r)
      DB.postEvent (database s) r
      maybe (return ()) (DB.postStore (database s)) (recordToMaybeStoreDB r)
      return s
    else do
      putStrLn $ "warning: received post record attempt from non-or-differently-authenticated connection"
      return s

possiblySendAllExerciseData :: ConnectionIndex -> Point -> Server -> IO ()
possiblySendAllExerciseData i (Point (Left (exId,ExerciseStarted)) t) s = do
  let h = snd ((connections s) ! i) -- should this be guarded against lookup failure, using getHandle like in getUserList below?
  if isJust h
    then getAllExerciseEvents i (fromJust h) exId s >> return ()
    else return ()
  return ()
possiblySendAllExerciseData _ _ _ = return ()

getUserList :: ConnectionIndex -> Server -> IO Server
getUserList i s = do
  let h = getHandle i s
  u <- maybe (return Nothing) (DB.findUser (database s)) h
  let r = maybe Nothing (Just . role) u
  if canSeeUserList r then do
    putStrLn $ "getUserList "
    allUsers <- DB.findAllUsers (database s)
    forM_ allUsers $ \(User uh _ ur) -> respond s i (UserData (User uh "" ur)) -- ie. blanking passwords before transmission
  else do
    putStrLn "warning: getUserList from non-authenticated connection"
  return s

getAllRecords :: ConnectionIndex -> Handle -> Server -> IO Server
getAllRecords i h s = do
  let h' = getHandle i s
  u <- maybe (return Nothing) (DB.findUser (database s)) h'
  let r = maybe Nothing (Just . role) u
  -- if authenticated as Administrator or Manager, or if authenticated as the user pertaining to the records, then proceed...
  if (canSeeUserList r || (h' == (Just h))) then do
    putStrLn $ "getAllRecords for " ++ h
    allRecords <- DB.findAllRecords (database s) h
    forM_ allRecords $ \x -> respond s i (RecordResponse x)
  else do
    putStrLn "warning: getUserList from non-authenticated connection"
  return s

getAllExerciseEvents :: ConnectionIndex -> Handle -> ExerciseId -> Server -> IO Server
getAllExerciseEvents i h e s = do
  let h' = getHandle i s
  u <- maybe (return Nothing) (DB.findUser (database s)) h'
  let r = maybe Nothing (Just . role) u
  -- if authenticated as Administrator or Manager, or if authenticated as the user pertaining to the records, then proceed...
  if (canSeeUserList r || (h' == (Just h))) then do
    allRecords <- DB.findAllExerciseEvents (database s) h e
    putStrLn $ "getAllExerciseEvents for " ++ h ++ " " ++ (show e) ++ ": " ++ (show (length allRecords)) ++ " values"
    forM_ allRecords $ \x -> respond s i (RecordResponse x)
  else do
    putStrLn "warning: getUserList from non-authenticated connection"
  return s


withServer :: MVar Server -> (Server -> IO Server) -> IO ()
withServer s f = takeMVar s >>= f >>= putMVar s

respond :: Server -> ConnectionIndex -> Response -> IO ()
respond s i x = WS.sendTextData c t
  where
    c = fst $ (Map.! i) $ (connections s)
    t = T.pack $ encode $ toJSON x

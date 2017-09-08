module InnerEar.Types.Server where

import qualified Network.WebSockets as WS
import Data.Map
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple

import InnerEar.Types.Handle
import InnerEar.Types.Password
import InnerEar.Types.User
import InnerEar.Types.Data
import InnerEar.Database.SQLite
import InnerEar.Database.Users
import InnerEar.Database.Events

type ConnectionIndex = Int

data Server = Server {
  database :: Connection,
  connections :: Map ConnectionIndex (WS.Connection, Maybe Handle),
  users :: Map Handle User
}

newServer :: Connection -> Server
newServer db = Server { database = db, connections = empty, users = empty}

addConnection :: WS.Connection -> Server -> (ConnectionIndex,Server)
addConnection c s = (i,s { connections = newMap })
  where i = head ([0..] Data.List.\\ (keys (connections s)))
        newMap = insert i (c,Nothing) (connections s)

deleteConnection :: ConnectionIndex -> Server -> Server
deleteConnection i s = s { connections = delete i (connections s)}


getPassword :: Handle -> Server -> Password
getPassword h s = password $ (users s) ! h

authenticateConnection :: ConnectionIndex -> Handle -> Server -> Server
authenticateConnection i h s = s { connections = newConnections } 
  where newConnections = adjust (\(ws,_) -> (ws,Just h)) i (connections s)

deauthenticateConnection :: ConnectionIndex -> Server -> Server
deauthenticateConnection i s = s { connections = newConnections }
  where newConnections = adjust (\(ws,_) -> (ws,Nothing)) i (connections s)

-- postPoint :: Handle -> Point -> Server -> Server
-- postPoint h p s = s { users = newUsers }
-- where newUsers = adjust (addPoint p) h (users s)

updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s

module InnerEar.Types.Server where

import qualified Network.WebSockets as WS
import Data.Map
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)

import InnerEar.Types.Handle
import InnerEar.Types.User

type ConnectionIndex = Int

data Server = Server {
  connections :: Map.Map ConnectionIndex (WS.Connection, Maybe Handle)
  users :: Map.Map Handle User
}

newServer :: Server
newServer = Server { connections = [], users = Map.empty }

addConnection :: WS.Connection -> Server -> (ConnectionIndex,Server)
addConnection c = (i,s { connections = newMap })
  where i = head ([0..] \\ Map.keys (connections s))
        newMap = Map.insert i (newUser c) (connections s)

deleteConnection :: ConnectionIndex -> Server -> Server
deleteConnection i s = s { connections = Map.delete i (connections s)}

updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s

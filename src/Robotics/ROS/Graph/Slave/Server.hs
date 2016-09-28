-- |
-- Module      :  Robotics.ROS.Graph.Slave.Server
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS slave XML-RPC server.
--
module Robotics.ROS.Graph.Slave.Server (slaveServer) where

import Snap.Http.Server (defaultConfig, setPort, Config, ConfigLog(..),
                         setVerbose, setAccessLog, setErrorLog,
                         simpleHttpServe)
import Data.Streaming.Network (getUnassignedPort)
import Control.Concurrent.Async (Async, async)
import Network.XmlRpc.Server (serve, fun)
import Network.HostName (getHostName)
import Data.Map (fromList)
import Data.Text (pack)
import Snap.Core (Snap)

import Robotics.ROS.Graph.Slave.Class
import Robotics.ROS.Graph.Internal

-- | Create 'Snap' XML-RPC handler from 'Slave' API.
slaveSnap :: Slave a => a -> Snap ()
slaveSnap s = serve $ fromList [
    ("getBusStats",      fun (getBusStats s))
  , ("getBusInfo",       fun (getBusInfo s))
  , ("getMasterUri",     fun (getMasterUri s))
  , ("shutdown",         fun (shutdown s))
  , ("getPid",           fun (getPid s))
  , ("getSubscriptions", fun (getSubscriptions s))
  , ("getPublications",  fun (getPublications s))
  , ("paramUpdate",      fun (paramUpdate s))
  , ("publisherUpdate",  fun (publisherUpdate s))
  , ("requestTopic",     fun (requestTopic s))
  ]

-- | Quiet Snap config with specified port.
quiet :: Int -> Config Snap ()
quiet port = setAccessLog ConfigNoLog .
             setErrorLog ConfigNoLog .
             setVerbose False .
             setPort port $
             defaultConfig

-- | Run ROS 'Slave' server and return 'Async' of server thread
-- and XML-RPC slave 'URI'.
slaveServer :: Slave a => a -> IO (Async (), URI)
slaveServer s = do
    host <- getHostName
    port <- getUnassignedPort
    let uri = concat ["http://", host, ":", show port]

    t <- async $ simpleHttpServe (quiet port) (slaveSnap s)
    return (t, pack uri)

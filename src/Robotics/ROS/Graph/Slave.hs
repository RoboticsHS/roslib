{-# LANGUAGE CPP, OverloadedStrings #-}
module Robotics.ROS.Graph.Slave
  ( Slave(..)
  , TopicConnection
  , runSlave
  , requestTopicClient
  ) where

import Control.Concurrent.Async (Async, async)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.UTF8 ()
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, setPort, Config, ConfigLog(..),
                                setVerbose, setAccessLog, setErrorLog)
import Snap.Core (Snap, readRequestBody, writeLBS, getResponse, putResponse, 
                  setContentLength)
import Data.Streaming.Network (getUnassignedPort)
import Network.XmlRpc.Internals (Value)
import Network.XmlRpc.Server (handleCall, methods, fun)
import Network.XmlRpc.Client (remote)
#ifndef mingw32_HOST_OS
import System.Posix.Process (getProcessID)
#endif
import Network.HostName (getHostName)
import Robotics.ROS.Graph.Stats (Stats(..))
import Robotics.ROS.Graph.Master
import Robotics.ROS.Types

type TopicConnection = (URI, Stats)
type StatSnapshot = [(TopicName, [TopicConnection])]

class Slave a where
    getMaster        :: a -> URI
    getNodeName      :: a -> String
    getNodeURI       :: MonadIO m => a -> m URI
    setNodeURI       :: MonadIO m => a -> URI -> m ()
    getSubscriptions :: a -> [(TopicName, TopicType)] 
    getPublications  :: a -> [(TopicName, TopicType)]
    getTopicPort     :: a -> TopicName -> Maybe Int
    getStats         :: MonadIO m => a -> m (StatSnapshot, StatSnapshot)
    publisherUpdate  :: MonadIO m => a -> TopicName -> [URI] -> m ()
    terminate        :: MonadIO m => a -> m ()
    waiting          :: MonadIO m => a -> m ()

#ifdef mingw32_HOST_OS
getProcessID :: IO Int
getProcessID = return 42
#endif

rpcResult :: a -> RPCResult a
rpcResult = return . (,,) 1 ""

mkPublishStats :: (TopicName, [TopicConnection]) -> 
                  (TopicName, Int, [(Int, Int, Int, Bool)])
mkPublishStats (n, pstats) = (n, 0, map formatStats pstats)
    where formatStats (_, (PubStats bytesSent numSent conn)) = 
              (0, bytesSent, numSent, conn)

mkSubStats :: (TopicName, [TopicConnection]) -> 
              (String, Int, [(Int, Int, Int, Bool)])
mkSubStats (n, sstats) = (n, 0, map formatStats sstats)
    where formatStats (_, (SubStats bytesReceived conn)) = 
              (0, bytesReceived, -1, conn)

getBusStats :: Slave a => a -> CallerID -> 
               RPCResult ([(String,Int,[(Int,Int,Int,Bool)])],
                          [(String,Int,[(Int,Int,Int,Bool)])],
                          (Int,Int,Int))
getBusStats n _ = do
    (subStats, pubStats) <- getStats n 
    rpcResult ( fmap mkPublishStats pubStats
              , fmap mkSubStats subStats
              , (0,0,0))

getBusInfo :: Slave a => a -> CallerID -> 
              RPCResult [(Int,String,String,String,String)]
getBusInfo n _ = do
    (subStats, pubStats) <- getStats n
    let pubBusInfo = concatMap (format "o") pubStats
        subBusInfo = concatMap (format "i") subStats
     in rpcResult $ pubBusInfo ++ subBusInfo
  where format direction (tname, stats) =
            fmap (\(uri,_) -> (0, uri, direction, "TCPROS", tname)) stats

getMaster' :: Slave a => a -> CallerID -> IO (Int, String, URI)
getMaster' n _ = rpcResult $ getMaster n

-- This requires a dependency on the unix package and so is not cross
-- platform.
getPid' :: CallerID -> RPCResult Int
getPid' _ = rpcResult =<< fromEnum <$> getProcessID

getSubscriptions' :: Slave a => a -> CallerID -> RPCResult [(String, String)]
getSubscriptions' n _ = rpcResult $ getSubscriptions n

getPublications' :: Slave a => a -> CallerID -> RPCResult [(String, String)]
getPublications' n _ = rpcResult $ getPublications n

-- TODO: Param update implementation
paramUpdate' :: Slave a => a -> CallerID -> String -> Value -> RPCResult Bool
paramUpdate' _n _ _paramKey _paramVal = do 
    putStrLn "WARN :: paramUpdate not implemented!"
    rpcResult True

pubUpdate :: Slave a => a -> CallerID -> TopicName -> [URI] -> RPCResult Int
pubUpdate n _ topic publishers = do publisherUpdate n topic publishers
                                    rpcResult 1

requestTopic :: Slave a => a -> CallerID -> TopicName -> [[Value]] -> 
                RPCResult (String, String, Int)
requestTopic n _ topic _protocols =
    case getTopicPort n topic of
      Just p -> do --putStrLn $ topic++" requested "++show p
                   host <- getHostName 
                   rpcResult ("TCPROS", host, p)
      Nothing -> return (0, "Unknown topic", ("TCPROS", "", 0))

-- Dispatch an XML-RPC request body and return the response. The first
-- parameter is a value that provides the necessary reflective API as
-- to ROS Node state. The second parameter is a semaphore indicating
-- that the node should terminate.
slave :: Slave a => a -> BLU.ByteString -> IO BLU.ByteString
slave n = handleCall dispatch . BLU.toString
  where dispatch = methods [ ("getBusStats", fun (getBusStats n))
                           , ("getBusInfo", fun (getBusInfo n))
                           , ("getMasterUri", fun (getMaster' n))
                           , ("getPid", fun getPid')
                           , ("getSubscriptions", fun (getSubscriptions' n))
                           , ("getPublications", fun (getPublications' n))
                           , ("paramUpdate", fun (paramUpdate' n))
                           , ("publisherUpdate", fun (pubUpdate n))
                           , ("requestTopic", fun (requestTopic n)) ]

-- |Start a Snap webserver on the specified port with the specified
-- handler.
snap :: Int -> Snap () -> IO ()
snap port handler = simpleHttpServe conf handler
  where conf :: Config Snap ()
        conf = setAccessLog ConfigNoLog .
               setErrorLog ConfigNoLog . 
               setVerbose False .
               setPort port $
               defaultConfig

-- |RPC handler for webserver
snapHandler :: (BLU.ByteString -> IO BLU.ByteString) -> Snap ()
snapHandler f = do
    body     <- readRequestBody 4096
    response <- liftIO $ f body
    writeLBS response
    let len = fromIntegral $ BLU.length response
     in putResponse . setContentLength len =<< getResponse

-- |Run a ROS slave node. Returns an action that will wait for the
-- node to shutdown along with the port the server is running on.
runSlave :: Slave a => a -> IO (Async (), Int)
runSlave n = do host  <- getHostName 
                port  <- getUnassignedPort 
                setNodeURI n $ "http://" ++ host ++ ":" ++ show port
                t <- async $ snap port $ snapHandler (slave n)
                return (t, port)

{-# LANGUAGE CPP #-}
-- |
-- Module      :  Robotics.ROS.Graph.Slave
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS slave API implemented in typeclass interface.
--
module Robotics.ROS.Graph.Slave.Class where

import Control.Monad.IO.Class (MonadIO(..))
import Network.XmlRpc.Internals (Value)
import Robotics.ROS.Graph.Internal
import Control.Monad (liftM)
import Robotics.ROS.Types
import Data.Text (Text)

#ifdef mingw32_HOST_OS
import System.Win32.Process (getProcessId)
getPID :: IO Int
getPID = liftM fromIntegral getProcessId
#else
import System.Posix.Process (getProcessID)
getPID :: IO Int
getPID = liftM fromIntegral getProcessID
#endif

-- | Get server process PID.
getPid' :: XReturn Int
getPid' = returnXR Ok =<< liftIO getPID

-- | ROS compatible protocol description.
-- [ProtocolName, ProtocolParam1, ProtocolParam2...N]
-- @protocolParams@ may be an empty list if there are no compatible protocols.
type Protocol = [Text]

-- | Transport topic statistics.
-- Stats is of the form [publishStats, subscribeStats, serviceStats] where
--
-- publishStats: [[topicName, messageDataSent, pubConnectionData]...]
-- subscribeStats: [[topicName, subConnectionData]...]
-- serviceStats: (proposed) [numRequests, bytesReceived, bytesSent]
--
-- pubConnectionData: [connectionId, bytesSent, numSent, connected]*
-- subConnectionData: [connectionId, bytesReceived, dropEstimate, connected]*
--
-- dropEstimate: -1 if no estimate.
--
type BusStats = ( [(TopicName, Int, [(Int, Int, Int, Bool)])]  -- Publish stats
                , [(TopicName, Int, [(Int, Int, Int, Bool)])]  -- Subscribe stats
                , (Int, Int, Int) )                            -- Service stats

-- | Transport connection information.
-- The busInfo is of the form:
-- [[connectionId1, destinationId1, direction1, transport1, topic1, connected1]... ]
--
--   * connectionId is defined by the node and is opaque
--
--   * destinationId is the XML-RPC URI of the destination
--
--   * direction is one of 'i', 'o', or 'b' (in, out, both)
--
--   * transport is the transport type (e.g. 'TCPROS')
--
--   * topic is the topic name
--
--   * connected1 indicates connection status. Note that this field is only
--     provided by slaves written in Python at the moment (cf. rospy/masterslave.py
--     in _TopicImpl.get_stats_info() vs. roscpp/publication.cpp
--     in Publication::getInfo()).
--
type BusInfo = [(Int, URI, Text, Text, TopicName)]

-- | ROS slave API
class Slave a where
    getBusStats      :: a -> CallerID -> XReturn BusStats
    -- ^ Retrieve transport/topic statistics.

    getBusInfo       :: a -> CallerID -> XReturn BusInfo
    -- ^ Retrieve transport/topic connection information.

    getMasterUri     :: a -> CallerID -> XReturn URI
    -- ^ Get the URI of the master node.

    shutdown         :: a -> CallerID -> Text -> XReturn Int
    -- ^ Stop this server.

    getPid           :: a -> CallerID -> XReturn Int
    -- ^ Get the PID of this server.
    getPid _ _ = getPid'

    getSubscriptions :: a -> CallerID -> XReturn [(TopicName, TopicType)]
    -- ^ Retrieve a list of topics that this node subscribes.

    getPublications  :: a -> CallerID -> XReturn [(TopicName, TopicType)]
    -- ^ Retrieve a list of topics that this node publishes.

    paramUpdate      :: a -> CallerID -> ParamName -> Value -> XReturn Int
    -- ^ Callback from master with updated value of subscribed parameter.

    publisherUpdate  :: a -> CallerID -> TopicName -> [URI] -> XReturn Int
    -- ^ Callback from master of current publisher list for specified topic.

    requestTopic     :: a -> CallerID -> TopicName -> [Protocol] -> XReturn [Protocol]
    -- ^ Publisher node API method called by a subscriber node. This requests
    -- that source allocate a channel for communication. Subscriber provides a
    -- list of desired protocols for communication. Publisher returns the
    -- selected protocol along with any additional params required for
    -- establishing connection. For example, for a TCP/IP-based connection,
    -- the source node may return a port number of TCP/IP server.

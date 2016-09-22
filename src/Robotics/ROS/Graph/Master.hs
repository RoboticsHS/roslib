-- |
-- Module      :  Robotics.ROS.Graph.Master
-- Copyright   :  Anthony Cowley 2010
--                Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Client functionality for the <http://wiki.ros.org/ROS/Master_API ROS Master API>.
--
module Robotics.ROS.Graph.Master where

import           Robotics.ROS.Graph.Internal
import           Network.XmlRpc.Client
import           Robotics.ROS.Types
import qualified Data.Text as T

-- | Subscribe the caller to the specified topic. In addition to
-- receiving a list of current publishers, the subscriber will also
-- receive notifications of new publishers via the publisherUpdate API.
registerSubscriber :: URI               -- ^ URI of ROS master
                   -> CallerID          -- ^ Caller ID
                   -> TopicName         -- ^ Topic name to register
                   -> TopicType         -- ^ Topic type, e.g. std_msgs/Int8
                   -> URI               -- ^ API URI of subscriber
                   -> XReturn [URI]     -- ^ List of XML-RPC API URIs of publishers
registerSubscriber url = remote (T.unpack url) "registerSubscriber"

-- | Unregister the caller as a subscriber of the topic.
unregisterSubscriber :: URI             -- ^ URI of ROS master
                     -> CallerID        -- ^ Caller ID
                     -> TopicName       -- ^ Topic name to unregister
                     -> URI             -- ^ API URI of subscriber
                     -> XReturn Int     -- ^ Zero when all done
unregisterSubscriber url = remote (T.unpack url) "unregisterSubscriber"

-- | Register the caller as a publisher the topic.
registerPublisher :: URI                -- ^ URI of ROS master
                  -> CallerID           -- ^ Caller ID
                  -> TopicName          -- ^ Topic name to register
                  -> TopicType          -- ^ Topic type, e.g. std_msgs/Int8
                  -> URI                -- ^ API URI of publisher
                  -> XReturn [URI]      -- ^ List of XML-RPC API URIs of subscribers
registerPublisher url = remote (T.unpack url) "registerPublisher"

-- | Unregister the caller as a publisher of the topic.
unregisterPublisher :: URI              -- ^ URI of ROS master
                    -> CallerID         -- ^ Caller ID
                    -> TopicName        -- ^ Topic name to unregister
                    -> URI              -- ^ API URI of publisher
                    -> XReturn Int      -- ^ Zero when all done
unregisterPublisher url = remote (T.unpack url) "unregisterPublisher"

{- What is it?
requestTopicClient :: URI
                   -> CallerID
                   -> TopicName
                   -> [[String]]
                   -> XReturn (String, String, Int)
requestTopicClient = flip remote "requestTopic"
-}

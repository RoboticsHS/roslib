-- |
-- Module      :  Robotics.ROS.Transport.Class
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS message transport typeclass.
--
module Robotics.ROS.Transport.Class where

import Robotics.ROS.Transport.Statistics (PublishStats, SubscribeStats)
import Robotics.ROS.Graph.Internal (TopicName, CallerID)
import Robotics.ROS.Graph.Slave.Class (TransportArgs)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO)
import Robotics.ROS.Msg.Class (Message)
import Pipes (Consumer, Producer)
import Data.String (IsString)
import Data.Text (pack)

-- | Publisher components: stats, pipe
type Publisher m a  = (PublishStats,   Consumer a m ())

-- | Subscriber components: stats, pipe
type Subscriber m a = (SubscribeStats, Producer a m ())

-- | ROS message transport
class (Show t, Read t) => Transport t where
    -- | Create a topic publisher
    publish   :: (MonadIO m, MonadLogger m, Message a)
              => t
              -- ^ Transport
              -> TopicName
              -- ^ Published topic name
              -> CallerID
              -- ^ Caller ident
              -> TransportArgs
              -- ^ Init arguments
              -> m (TransportArgs, Publisher m a)
              -- ^ Transport arguments and publisher

    -- | Create a topic subscriber
    subscribe :: (MonadIO m, MonadLogger m, Message a)
              => t
              -- ^ Transport
              -> TopicName
              -- ^ Subscribed topic name
              -> CallerID
              -- ^ Caller ident
              -> m (TransportArgs -> m (), Subscriber m a)
              -- ^ Transport arguments catcher and subscriber

    -- | Subscriber arguments getter
    subscribeArgs :: t -> TransportArgs

-- | Create request topic arguments from transport
transportArgs :: Transport a => a -> TransportArgs
transportArgs t = pack (show t) : subscribeArgs t

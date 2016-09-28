{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Robotics.ROS.Transport.Statistics
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS transport statistic utils.
--
module Robotics.ROS.Transport.Statistics (
  -- ** Publisher statistics
    PublishStats(..)
  , newPublishStats
  -- ** Subscriber statistics
  , SubscribeStats(..)
  , newSubscribeStats
  -- ** Connection container
  , ConnectionID
  , TopicConnections
  -- ** Statistics storage and tools
  , Statistics
  , statisticPubs
  , statisticSubs
  , appendPublishStats
  , appendSubscribeStats
  , snapshot
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Robotics.ROS.Graph.Slave.Class (BusStats)
import           Robotics.ROS.Graph.Internal (TopicName)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad (liftM)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid

-- | Publish connection statistics
data PublishStats = PublishStats
  { bytesSent    :: MVar Int
  , numSent      :: MVar Int
  , pubConnected :: MVar Int
  }

-- | 'PublishStats' creator
newPublishStats :: MonadIO m => m PublishStats
newPublishStats =
    PublishStats <$> liftIO (newMVar 0)
                 <*> liftIO (newMVar 0)
                 <*> liftIO (newMVar 0)

-- | Subscribe connection statistics
data SubscribeStats = SubscribeStats
  { bytesRecv    :: MVar Int
  , dropEstimate :: MVar Int
  , subConnected :: MVar Int
  }

-- | 'SubscribeStats' creator
newSubscribeStats :: MonadIO m => m SubscribeStats
newSubscribeStats =
    SubscribeStats <$> liftIO (newMVar 0)
                   <*> liftIO (newMVar (-1))
                   <*> liftIO (newMVar 0)

-- | Integral connection indentifier
type ConnectionID = Int

-- | Mapping from 'TopicName' and 'ConnectionID' to stats
type TopicConnections a = Map TopicName (Map ConnectionID a)

-- | The statistics storage
data Statistics = Statistics
  { statisticPubs :: TopicConnections PublishStats
  -- ^ Publish bus accounting table
  , statisticSubs :: TopicConnections SubscribeStats
  -- ^ Subscribe bus accounting table
  }

instance Monoid Statistics where
    mempty = Statistics mempty mempty
    mappend (Statistics sp1 ss1) (Statistics sp2 ss2) =
        Statistics (M.union sp1 sp2) (M.union ss1 ss2)

-- | Make new uniq connection ident
mkConnectionID :: Statistics -> ConnectionID
mkConnectionID s = getSum $
     foldMap (Sum . M.size) (statisticPubs s)
  <> foldMap (Sum . M.size) (statisticSubs s)

-- | Insert new record into 'TopicConnections'
insertTC :: a
         -> TopicName
         -> ConnectionID
         -> TopicConnections a
         -> TopicConnections a
insertTC x tname cid v
    | tname `M.member` v = M.update (Just . M.insert cid x) tname v
    | otherwise          = M.insert tname (M.singleton cid x) v

-- | Append 'PublishStats' and update 'Statistics'
appendPublishStats :: TopicName
                   -> PublishStats
                   -> Statistics
                   -> Statistics
appendPublishStats tname stats s = s { statisticPubs = pubs }
  where cid  = mkConnectionID s
        pubs = insertTC stats tname cid (statisticPubs s)

-- | Append 'SubscribeStats' and update 'Statistics'
appendSubscribeStats :: TopicName
                     -> SubscribeStats
                     -> Statistics
                     -> Statistics
appendSubscribeStats tname stats s = s { statisticSubs = subs }
  where cid  = mkConnectionID s
        subs = insertTC stats tname cid (statisticSubs s)

-- | Take a snapshot of 'Statistics' storage as 'BusStats'.
snapshot :: MonadIO m => Statistics -> m BusStats
snapshot s = (,,)
    <$> mapM snapshotPublish   (M.toList (statisticPubs s))
    <*> mapM snapshotSubscribe (M.toList (statisticSubs s))
    <*> return (0, 0, 0)  -- TODO: Service stats
  where snapshotPublish (tname, cdata) = do
            d <- mapM extractPublishData (M.toList cdata)
            let sumSent = foldMap (\(_, _, a, _) -> Sum a) d
            return (tname, getSum sumSent, d)

        extractPublishData (cid, c) = (,,,)
            <$> return cid
            <*> liftIO (readMVar (bytesSent c))
            <*> liftIO (readMVar (numSent c))
            <*> liftM (>0) (liftIO (readMVar (pubConnected c)))

        snapshotSubscribe (tname, cdata) = do
            d <- mapM extractSubscribeData (M.toList cdata)
            return (tname, d)

        extractSubscribeData (cid, c) = (,,,)
            <$> return cid
            <*> liftIO (readMVar (bytesRecv c))
            <*> liftIO (readMVar (dropEstimate c))
            <*> liftM (>0) (liftIO (readMVar (subConnected c)))

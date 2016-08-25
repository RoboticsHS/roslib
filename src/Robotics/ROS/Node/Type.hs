{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Robotics.ROS.Node.Type 
  ( Node(..)
  , NodeConfig(..)
  , NodeState(..)
  , Params
  , Remap
  ) where

import Control.Concurrent.STM (atomically, TVar, readTVar, writeTVar)
import Control.Concurrent (MVar, readMVar, putMVar)
import Control.Concurrent.Async (Async, wait, cancel)
import Data.Conduit (Sink, Source)
import Data.Maybe (catMaybes)
import Data.Binary (Binary)
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Robotics.ROS.Node.ArgRemapping (ParamVal)
import Robotics.ROS.Graph.Slave (Slave(..))
import Robotics.ROS.Graph.Stats
import Robotics.ROS.Msg.MsgInfo
import Robotics.ROS.Types

type Port = Int
type Connections = TVar [URI]

data Topic 
  = Subscription TopicName TopicType Connections (Async ())
  | Publication  TopicName TopicType Connections Port (Async ())

data NodeState = NodeState { name           :: String
                           , master         :: URI
                           , namespace      :: String
                           , nodeURI        :: MVar URI
                           , statistics     :: StatsMap
                           , topics         :: Map TopicName Topic
                           }

type Params = [(String, ParamVal)]
type Remap  = [(String, String)]

data NodeConfig = NodeConfig { nodeParams :: Params
                             , nodeRemaps :: Remap
                             }

-- |A 'Node' carries with it parameters, topic remappings, and some
-- state encoding the status of its subscriptions and publications.
newtype Node a = Node { unNode :: ReaderT NodeConfig (StateT NodeState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState NodeState Node where
    get = Node get
    put = Node . put

instance MonadReader NodeConfig Node where
    ask = Node ask
    local f m = Node $ withReaderT f (unNode m)

instance Slave NodeState where
    getMaster   = master
    getNodeName = name

    getNodeURI      = liftIO . readMVar . nodeURI
    setNodeURI node = liftIO . putMVar (nodeURI node)
 
    getSubscriptions = catMaybes . M.elems . fmap toSubscription . topics
      where toSubscription (Subscription tname ttype _ _) = Just (tname, ttype)
            toSubscription _ = Nothing

    getPublications = catMaybes . M.elems . fmap toPublication . topics
      where toPublication (Publication tname ttype _ _ _) = Just (tname, ttype)
            toPublication _ = Nothing

    getStats n =
        liftIO $ atomically $ do
            stat <- readTVar $ statistics n
            subsSnapshot <- mapM (mapM readTVar) $ subscriptions $ topics n
            pubsSnapshot <- mapM (mapM readTVar) $ publications  $ topics n
            statSnapshot <- mapM readTVar stat
            let subStats = fmap (zipStats statSnapshot) <$> subsSnapshot
                pubStats = fmap (zipStats statSnapshot) <$> pubsSnapshot
             in return (subStats, pubStats)

      where toSubscription (Subscription tname _ uris _) = Just (tname, uris)
            toSubscription _ = Nothing

            toPublication (Publication tname _ uris _ _) = Just (tname, uris)
            toPublication _ = Nothing

            subscriptions = catMaybes . M.elems . fmap toSubscription
            publications  = catMaybes . M.elems . fmap toPublication

            zipStats stats uris = M.toList $ M.filterWithKey (\k _ -> k `elem` uris) stats

    publisherUpdate node name connections =
        case M.lookup name (topics node) of
            Just (Publication _ _ c _ _) -> liftIO $ 
                atomically $ writeTVar c connections
            _ -> return ()

    getTopicPort node name =
        case M.lookup name (topics node) of
            Just (Publication _ _ _ p _) -> Just p
            _ -> Nothing

    waiting = liftIO . mapM_ waitTopic . topics
      where waitTopic (Publication _ _ _ _ a) = wait a
            waitTopic (Subscription _ _ _ a) = wait a

    terminate = liftIO . mapM_ cancelTopic . topics 
      where cancelTopic (Publication _ _ _ _ a) = cancel a
            cancelTopic (Subscription _ _ _ a) = cancel a


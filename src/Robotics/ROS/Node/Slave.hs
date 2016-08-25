module Robotics.ROS.Node.Slave (slave) where

import System.Posix.Signals (installHandler, Handler(..), sigINT)
import Control.Concurrent.Async (cancel, wait)
import Control.Monad.IO.Class
import Robotics.ROS.Graph.Master
import Robotics.ROS.Graph.Slave
import Robotics.ROS.Types

registerSlave :: (Slave node, MonadIO m) => String -> node -> m ()
registerSlave name n = do
    uri <- getNodeURI n
    mapM_ (regPub uri) $ getPublications n
    mapM_ (regSub uri) $ getSubscriptions n
  where
    master = getMaster n 
    -- Inform the master that we are publishing a particular topic.
    regPub uri (tname, ttype) = liftIO $
        registerPublisher master name tname ttype uri
    -- Inform the master that we are subscribing to a particular topic.
    regSub uri (tname, ttype) = liftIO $ do 
        (r, _, publishers) <- registerSubscriber master name tname ttype uri
        if r == 1
            then publisherUpdate n tname publishers
            else error "Failed to register subscriber with master"

-- |Run a ROS Node with the given name. Returns when the Node has
-- shutdown either by receiving an interrupt signal (e.g. Ctrl-C) or
-- because the master told it to stop.
slave :: (Slave node, MonadIO m) => String -> node -> m ()
slave name n = do
    (rpc, _) <- liftIO $ runSlave n 
    registerSlave name n
    let shutdown = do putStrLn "Shutting down"
                      terminate n
                      cancel rpc
    liftIO $ do
        installHandler sigINT (CatchOnce shutdown) Nothing
        waiting n
        wait rpc

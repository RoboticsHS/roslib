-- |
-- Module      :  Robotics.ROS.Transport.ROSTCP
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- TCPROS is a transport layer for ROS Messages and Services. It
-- uses standard TCP/IP sockets for transporting message data. Inbound
-- connections are received via a TCP Server Socket with a header containing
-- message data type and routing information. See
-- <http://wiki.ros.org/ROS/ROSTCP ROSTCP wiki page>
--
module Robotics.ROS.Transport.ROSTCP (ROSTCP) where

import Robotics.ROS.Graph.Slave.Class (TransportArgs)
import Data.Streaming.Network (getUnassignedPort)
import Robotics.ROS.Transport.ConnectionHeader
import Control.Monad.State.Strict (evalStateT)
import Robotics.ROS.Transport.Accounting
import Robotics.ROS.Transport.Statistics
import Control.Monad (forever, when)
import Robotics.ROS.Transport.Codec
import Robotics.ROS.Transport.Class
import Robotics.ROS.Graph.Internal
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Robotics.ROS.Msg.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Binary (encode)
import Data.Monoid ((<>))
import Control.Exception
import Pipes.Network.TCP
import Pipes.ByteString
import Data.Text as T
import Data.Map as M
import Pipes

data ROSTCP = ROSTCP deriving (Show, Eq, Read)

instance Transport ROSTCP where
    publish _ n c   = mkPublisher (fromNameCaller n c)
    subscribe _ n c = mkSubscriber (fromNameCaller n c)
    subscribeArgs _ = []

-- | Topic abstraction as pair of name and sample message
type Topic a = (TopicName, CallerID, a)

-- | Make Topic from name
fromNameCaller :: Message a => TopicName -> CallerID -> Topic a
fromNameCaller n c = (n, c, undefined)

-- | Make a 'Consumer' from 'Chan'
toChan :: MonadIO m => Chan a -> Consumer a m ()
toChan c = forever (await >>= liftIO . writeChan c)

-- | Make a 'Producer' from 'Chan'
fromChan :: MonadIO m => Chan a -> Producer a m ()
fromChan c = forever (liftIO (readChan c) >>= yield)

-- | Safe catch exceptions

-- | Create publisher for given topic
mkPublisher :: (MonadIO m, MonadLogger m, Message a)
            => Topic a
            -> TransportArgs
            -> m (TransportArgs, Publisher m a)
mkPublisher t@(tname, _, _) _ = do
    stat <- newPublishStats
    port <- show <$> liftIO getUnassignedPort
    sock <- initPubSocket t port (pubConnected stat)

    let args  = [T.pack port]
        bytes = bytesMeter (bytesSent stat)
        count = countMeter (numSent stat)
        pipe  = count >-> encoder >-> bytes >-> sock

    logDebugNS ("ROSTCP[" <> tname <> "]")
               ("Publisher initialized with " <> T.pack (show args))
    return (args, (stat, pipe))

-- | Port number
type Port = String

-- | Start TCP server wiht handshaker and consumer fusion
initPubSocket :: (MonadIO m, Message a)
              => Topic a
              -> Port
              -> MVar Int
              -> m (Consumer ByteString m ())
initPubSocket t port cn = do
    chan <- liftIO newChan
    serve HostAny port $ \(sock, _) -> do
        catchExceptions $ do
            -- Handshake
            negotiatePub t sock
            -- Increment count of connections
            varAdd cn 1
            -- Relay message data
            c <- dupChan chan
            forever (readChan c >>= send sock)
        -- Decrement count of connections
        varAdd cn (-1)
    return (toChan chan)

negotiatePub :: Message a => Topic a -> Socket -> IO ()
negotiatePub (tname, callerid, msg) sock = do
    -- Parse request header
    reqHeader <- evalStateT headerDecoder (fromSocket sock 4096)
    let header = unConnectionHeader reqHeader

    -- Type check
    when (header ! "type" /= getType msg) $
        throwIO . userError . T.unpack $
            "Disagreeing Topic types: "
         <> "publisher expected " <> getType msg
         <> ", but client asked for " <> header ! "type"

    -- MD5 check
    when (header ! "md5sum" /= md5 msg) $
        throwIO . userError . T.unpack $
            "Disagreement on Topic MD5: "
         <> "publisher expected " <> md5 msg
         <> ", but client asked for " <> header ! "md5sum"

    -- Response header
    runEffect $ yield respHeader >-> headerEncoder >-> toSocket sock
  where md5 = T.pack . show . getDigest
        respHeader = ConnectionHeader $ M.fromList
            [ ("md5sum", md5 msg)
            , ("type", getType msg)
            , ("callerid", callerid)
            ]

-- TODO: MonadLogger
catchExceptions :: IO () -> IO ()
catchExceptions = flip catch logError
  where logError :: SomeException -> IO ()
        logError e = putStrLn ("ROSTCP Exception: " <> displayException e)

-- | Create connector and subscriber for given topic
mkSubscriber :: (MonadIO m, MonadLogger m, Message a)
             => Topic a -> m (TransportArgs -> m (), Subscriber m a)
mkSubscriber t = do
    stat <- newSubscribeStats
    chan <- liftIO newChan
    return (initSubSocket t chan stat, (stat, fromChan chan))

initSubSocket :: (MonadIO m, MonadLogger m, Message a)
              => Topic a -> Chan a -> SubscribeStats -> TransportArgs -> m ()
initSubSocket t@(tname, _, _) chan stat [host, port] = do
    liftIO $ forkIO $ do
        connect (T.unpack host) (T.unpack port) $ \(sock, _) -> do
            catchExceptions $ do
                -- Handshake
                negotiateSub t sock
                -- Increment count of connections
                varAdd (subConnected stat) 1
                -- Decode message and relay
                let bytes = bytesMeter (bytesRecv stat)
                    pipe  = decoder (fromSocket sock 4096 >-> bytes)
                c <- dupChan chan
                runEffect (pipe >-> toChan c)
        -- Decrement count of connections
        varAdd (subConnected stat) (-1)
    logDebugNS ("ROSTCP[" <> tname <> "]")
               ("Subscriber initialized with " <> T.pack (show [host, port]))

negotiateSub :: Message a => Topic a -> Socket -> IO ()
negotiateSub (tname, callerid, msg) sock = do
    -- Send header
    runEffect $ yield reqHeader >-> headerEncoder >-> toSocket sock

    -- Parse response header
    respHeader <- evalStateT headerDecoder (fromSocket sock 4096)
    let header = unConnectionHeader reqHeader

    -- Type check
    when (header ! "type" /= getType msg) $
        throwIO . userError . T.unpack $
            "Disagreeing Topic types: "
         <> "publisher expected " <> getType msg
         <> ", but client asked for " <> header ! "type"

    -- MD5 check
    when (header ! "md5sum" /= md5 msg) $
        throwIO . userError . T.unpack $
            "Disagreement on Topic MD5: "
         <> "publisher expected " <> md5 msg
         <> ", but client asked for " <> header ! "md5sum"
  where md5 = T.pack . show . getDigest
        reqHeader = ConnectionHeader $ M.fromList
            [ ("md5sum", md5 msg)
            , ("type", getType msg)
            , ("topic", tname)
            , ("callerid", callerid)
            , ("message_definition", getSource msg)
            , ("tcp_nodelay", "1")
            ]

module Robotics.ROS.Node.ROSTCP
  ( runServer
  , runServers
  ) where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Streaming.Network (runTCPServer)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Char8 (ByteString)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Conduit.List as CL
import Control.Monad (when)
import Data.Conduit.Network
import Data.Conduit
import Robotics.ROS.Types
import Robotics.ROS.Msg.MsgInfo
import Robotics.ROS.Node.ConnectionHeader

publisher :: (MsgInfo a, Binary a, MonadIO m) => a -> AppData -> m (Sink a m ())
publisher msg ad = do
    appSource ad $= handshakePub ttype md5 $$ appSink ad
    return $ CL.map (toStrict . encode) =$ appSink ad
  where ttype = msgTypeName msg
        md5   = sourceMD5 msg

handshakePub :: MonadIO m => String -> String -> Conduit ByteString m ByteString
handshakePub ttype md5 = do
    x <- fmap (decode . fromStrict) <$> await
    case x of
        Nothing -> return () 
        Just (ConnectionHeader header) -> do
            let wildCard = case lookup "type" header of
                    Just t | t == "*" -> True
                           | t == ttype -> False
                           | otherwise -> error $ "Disagreeing Topic types: " ++
                                                  "publisher expected "++ttype++
                                                  ", but client asked for "++t
                    Nothing -> error $ "Client did not include the "++
                                       "topic type in its "++
                                       "connection request."

            when (not wildCard)
                (case lookup "md5sum" header of
                    Just s | s == md5 -> return ()
                           | otherwise -> error "Disagreement on Topic type MD5"
                    Nothing -> error $ "Client did not include MD5 sum "++
                                       "in its request.")

            yield $ toStrict . encode $
                ConnectionHeader [ ("md5sum", md5)
                                 , ("type", ttype)
                                 , ("callerid", "roshask")]

runServer = undefined
runServers = undefined

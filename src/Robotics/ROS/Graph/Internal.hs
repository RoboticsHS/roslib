{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      :  Robotics.ROS.Graph.Internal
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Internal and common Graph module staff.
--
module Robotics.ROS.Graph.Internal where

import Control.Monad.Error.Class (MonadError(..))
import Network.XmlRpc.Internals (XmlRpcType)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import Data.Text (Text)

-- | URI of any thing, e.g. for ROS master http://localhost:11311
type URI = Text

-- | ROS caller_id is fully-qualified name of local node.
-- _caller_id = _caller_namespace+'unnamed' #default for non-node.
type CallerID = Text

-- | ROS node parameter name.
type ParamName = Text

-- | ROS topic name.
type TopicName = Text

-- | ROS topic type string, e.g std_msgs/UInt8.
type TopicType = Text

-- | ROS master XML-RPC result type.
-- Return values are lists in the format: [statusCode, statusMessage, value]
--
--   * statusCode (int): An integer indicating the completion condition of the
--     method. Current values:
--
--       -1: ERROR: Error on the part of the caller, e.g. an invalid parameter.
--           In general, this means that the master/slave did not attempt to
--           execute the action.
--
--        0: FAILURE: Method failed to complete correctly. In general, this means
--           that the master/slave attempted the action and failed, and there may
--           have been side-effects as a result.
--
--        1: SUCCESS: Method completed successfully.
--
--     Individual methods may assign additional meaning/semantics to statusCode.
--
--   * statusMessage (str): a human-readable string describing the return status
--
--   * value (anytype): return value is further defined by individual API calls.
type XReturn a = IO (Int, Text, a)

-- | ROS XML-RPC return code.
data XReturnCode = Error | Failure | Ok
  deriving (Show, Enum)

-- | Helper for extracting a value from a tuple returned by the
-- server.
handleXR :: (MonadIO m, MonadError Text m, XmlRpcType a)
         => XReturn a -> m a
handleXR mr = do
    (code, str, v) <- liftIO mr
    case toEnum (code + 1) of
        Ok -> return v
        _  -> throwError ("ROS master error: " <> str)

-- | The 'XResult' creator from error code and value.
returnXR :: XmlRpcType a => XReturnCode -> a -> XReturn a
returnXR c v = case c of
    Ok -> return (1, "", v)
    _  -> return (0, "An error occured", v)

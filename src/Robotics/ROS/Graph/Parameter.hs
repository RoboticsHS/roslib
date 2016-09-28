-- |
-- Module      :  Robotics.ROS.Graph.Parameter
-- Copyright   :  Anthony Cowley 2010
--                Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Client interface for the ROS Parameter Server API. Note that
-- dictionary values are not supported.
--
module Robotics.ROS.Graph.Parameter
  ( deleteParam
  , setParam
  , getParam
  , searchParam
  , subscribeParam
  , unsubscribeParam
  , hasParam
  , getParamNames
  ) where

import           Network.XmlRpc.Internals (XmlRpcType)
import           Robotics.ROS.Graph.Internal
import           Network.XmlRpc.Client
import qualified Data.Text as T

-- | Delete a parameter on the server.
deleteParam :: URI              -- ^ ROS master URI
            -> CallerID         -- ^ Caller ID
            -> ParamName        -- ^ Parameter name
            -> XReturn Int      -- ^ Zero when all done
deleteParam url = remote (T.unpack url) "deleteParam"

-- | Set a parameter value on the server.
setParam :: XmlRpcType a => URI             -- ^ ROS master URI
                         -> CallerID        -- ^ Caller ID
                         -> ParamName       -- ^ Parameter name
                         -> a               -- ^ Parameter value
                         -> XReturn Int     -- ^ Zero when all done
setParam url = remote (T.unpack url) "setParam"

-- | Retrieve parameter value from server.
getParam :: XmlRpcType a => URI             -- ^ ROS master URI
                         -> CallerID        -- ^ Caller ID
                         -> ParamName       -- ^ Parameter name
                         -> XReturn a       -- ^ Parameter value
getParam url = remote (T.unpack url) "getParam"

-- | Search for a parameter name on the Parameter Server. The search
-- starts in the caller's namespace and proceeds upwards through
-- parent namespaces until the Parameter Server finds a matching
-- key. The first non-trivial partial match is returned.
searchParam :: URI                  -- ^ ROS master URI
            -> CallerID             -- ^ Caller ID
            -> ParamName            -- ^ Parameter name
            -> XReturn ParamName    -- ^ Search result
searchParam url = remote (T.unpack url) "searchParam"

-- | Retrieve parameter value from server and subscribe to updates to
-- that param. See paramUpdate in the '?' API.
subscribeParam :: XmlRpcType a => URI           -- ^ ROS master URI
                               -> CallerID      -- ^ Caller ID
                               -> URI           -- ^ XML-RPC URI of subscriber
                               -> ParamName     -- ^ Parameter name
                               -> XReturn a     -- ^ Parameter value
subscribeParam url = remote (T.unpack url) "subscribeParam"

-- | Unsubscribe from updates to a parameter.
unsubscribeParam :: URI             -- ^ ROS master URI
                 -> CallerID        -- ^ Caller ID
                 -> URI             -- ^ XML-RPC URI of subscriber
                 -> ParamName       -- ^ Parameter name
                 -> XReturn Int     -- ^ Zero when done
unsubscribeParam url = remote (T.unpack url) "unsubscribeParam"

-- | Check if a parameter is stored on the server.
hasParam :: URI             -- ^ ROS master URI
         -> CallerID        -- ^ Caller ID
         -> ParamName       -- ^ Parameter name
         -> XReturn Bool    -- ^ 'True' when parameter exist
hasParam url = remote (T.unpack url) "hasParam"

-- | Get a list of all parameter names stored on this server.
getParamNames :: URI                    -- ^ ROS master URI
              -> CallerID               -- ^ Caller ID
              -> XReturn [ParamName]    -- ^ List of registered parameters
getParamNames url = remote (T.unpack url) "getParamNames"

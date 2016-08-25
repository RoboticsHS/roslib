-- |Utility types for working with ROS.
module Robotics.ROS.Types where

import Data.Word (Word32)

type URI          = String
type CallerID     = String
type TopicName    = String
type NodeName     = String
type ParamName    = String
type TopicType    = String
type ConnectionID = Int
type ServiceName  = String

-- |XML-RPC result type
type RPCResult a = IO (Int, String, a)

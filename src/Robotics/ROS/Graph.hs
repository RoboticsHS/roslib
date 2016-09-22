-- |
-- Module      :  Robotics.ROS.Graph
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- The ROS Master and Slave APIs manage information about the availability of
-- topics and services for ROS, as well as the negotiation of connection
-- transport. They are implemented using XML-RPC and come in a variety of
-- language implementations.
--
module Robotics.ROS.Graph (
  -- * Parameter server API
    setParam
  , getParam
  , deleteParam
  , searchParam
  , subscribeParam
  , unsubscribeParam
  , hasParam
  , getParamNames
  -- * ROS master API
  , registerSubscriber
  , unregisterSubscriber
  , registerPublisher
  , unregisterPublisher
  -- * ROS slave API
  , Slave(..)
  , runSlave
  ) where

import Robotics.ROS.Graph.Parameter
import Robotics.ROS.Graph.Master
import Robotics.ROS.Graph.Slave

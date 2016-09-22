-- |
-- Module      :  Robotics.ROS.Graph.Slave
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Client functionality for the <http://wiki.ros.org/ROS/Slave_API ROS Slave API>.
--
module Robotics.ROS.Graph.Slave (
  -- * ROS slave API
    Slave(..)
  , Protocol
  , BusStats
  , BusInfo
  -- * ROS slave server
  , runSlave
  ) where

import Robotics.ROS.Graph.Slave.Class
import Robotics.ROS.Graph.Slave.Server

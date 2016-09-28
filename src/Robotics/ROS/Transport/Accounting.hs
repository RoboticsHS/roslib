-- |
-- Module      :  Robotics.ROS.Transport.Accounting
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Pipes transfer bytes/count accounting. Pipes injectors used for
-- accounting ROS topic statistics.
--
module Robotics.ROS.Transport.Accounting (
    CountMeter
  , BytesMeter
  , countMeter
  , bytesMeter
  , varAdd
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Data.ByteString as BS
import Pipes

-- | Pipe for accounting count of passed items
type CountMeter a m = Pipe a a m ()

-- | Pipe for accounting count of passed bytes
type BytesMeter m = Pipe ByteString ByteString m ()

-- | Add value to numeric var.
varAdd :: (MonadIO m, Num a) => MVar a -> a -> m ()
varAdd var x = liftIO $ modifyMVar_ var (return . (x +))

-- | Count of 'Message' transfer accounting.
countMeter :: (MonadIO m, Num n)
           => MVar n            -- ^ Accumulator
           -> CountMeter a m    -- ^ Injector pipe
countMeter var = do
    _ <- cat
    varAdd var 1
    countMeter var

-- | Lazy ByteString bytes transfer accounting.
bytesMeter :: (MonadIO m, Num n)
           => MVar n            -- ^ Accumulator
           -> BytesMeter m      -- ^ Injector pipe
bytesMeter var = do
    str <- await
    yield str
    varAdd var (sizeOf str)
    bytesMeter var
  where sizeOf = fromIntegral . BS.length

{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      :  Robotics.ROS.Transport.Codec
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Binary encoder/decoder pipes.
--
module Robotics.ROS.Transport.Codec where

import Control.Monad.State.Strict (evalStateT)
import Pipes.ByteString (ByteString, drop)
import Control.Exception (throwIO)
import Prelude hiding (drop)
import Data.Text (pack)
import Pipes.Binary
import Pipes

-- | 'Binary' encoder pipe
encoder :: (Binary a, Monad m) => Pipe a ByteString m ()
encoder = for cat encode

-- | 'Producer' decoder for 'Binary' instances
decoder :: (Binary a, MonadIO m)
        => Producer ByteString m () -> Producer a m ()
decoder p = do
    r <- lift (evalStateT decodeL p)
    case r of
        Left e -> liftIO (throwIO (userError (deMessage e)))
        Right (offset, x) -> do
            yield x
            decoder (drop offset p)

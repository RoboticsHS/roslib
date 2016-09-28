-- |
-- Module      :  Robotics.ROS.Transport.ConnectionHeader
-- Copyright   :  Anthony Cowley 2010
--                Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- The ROS connection header contains important metadata about a
-- connection being established, including typing information and
-- routing information. How it is exchanged depends on the ROS
-- transport being used.
--
module Robotics.ROS.Transport.ConnectionHeader (
    ConnectionHeader(..)
  , headerDecoder
  , headerEncoder
  ) where

import Data.Binary.Get (Get, runGet, getByteString, getWord32le, isEmpty)
import Data.Binary.Put (Put, putByteString, putWord32le)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Lazy (fromChunks)
import Lens.Family2.State.Strict (zoom)
import Data.Map (Map, fromList, toList)
import Data.Binary (Binary, put, get)
import Data.ByteString as B (length)
import Data.ByteString (ByteString)
import Control.Exception (throwIO)
import Pipes.ByteString (splitAt)
import Prelude hiding (splitAt)
import Data.Text (Text, split)
import Pipes.ByteString as BS
import Pipes.Parse (drawAll)
import Control.Monad (liftM)
import Data.Monoid ((<>))
import Data.Word (Word32)
import Pipes.Binary
import Pipes

-- | Wrapper for the Connection Header type that is a list of key-value
-- pairs.
newtype ConnectionHeader = ConnectionHeader
  { unConnectionHeader :: Map Text Text }
  deriving (Show, Eq)

-- | Binary instance for Connection Header serialization
instance Binary ConnectionHeader where
    put = putConnectionHeader
    get = getConnectionHeader

-- | Prefix a ByteString with its length encoded as a 4-byte little
-- endian integer.
tagLength :: ByteString -> Put
tagLength = putWord32le . fromIntegral . B.length

-- | Serialize a list of key-value pairs using the ROS Connection
-- Header protocol.
putConnectionHeader :: ConnectionHeader -> Put
putConnectionHeader = go . toList . unConnectionHeader
  where go [] = return ()
        go ((name, value) : xs) = do
            let field = encodeUtf8 (name <> "=" <> value)
            tagLength     field
            putByteString field
            go xs

-- | Each entry in the header is a 4-byte little endian integer denoting
-- the length of the entry, the field name string, an equals sign, and
-- the field value string.
parsePair :: Get (Text, Text)
parsePair = do len   <- getWord32le
               field <- getByteString (fromIntegral len)
               case split (== '=') (decodeUtf8 field) of
                 [key, value] -> return (key, value)
                 _ -> fail ("ConnectionHeader field broken: " ++ show field)

-- | Keep parsing header entries until we run out of bytes.
getConnectionHeader :: Get ConnectionHeader
getConnectionHeader = liftM (ConnectionHeader . fromList) (go [])
  where go acc = do empty <- isEmpty
                    if empty
                      then return acc
                      else go =<< (: acc) <$> parsePair

headerDecoder :: Parser ByteString IO ConnectionHeader
headerDecoder = do
    len <- liftM decodeLen (zoom (splitAt 4) drawAll)
    r <- zoom (splitAt len) decode
    case r of
        Left e -> lift (throwIO e)
        Right x -> return x
  where decodeLen = runGet getWord32le . fromChunks

headerEncoder :: Monad m => Pipe ConnectionHeader ByteString m ()
headerEncoder = do
    h <- await
    len <- BS.length (encode h)
    encode (len :: Word32)
    encode h

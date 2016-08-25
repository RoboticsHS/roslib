-- |The ROS connection header contains important metadata about a
-- connection being established, including typing information and
-- routing information. How it is exchanged depends on the ROS
-- transport being used.
module Robotics.ROS.Node.ConnectionHeader
  ( ConnectionHeader(..)
  ) where

import Data.Binary.Get (Get, getByteString, getWord32le, isEmpty)
import Data.Binary.Put (Put, putByteString, putWord32le)
import Data.Binary (Binary, put, get)
import Data.ByteString.Char8 as B
import Control.Arrow ((***))

-- |Wrapper for the Connection Header type that is a list of key-value
-- pairs.
newtype ConnectionHeader = ConnectionHeader [(String, String)]
  deriving (Show, Eq)

-- |Binary instance for Connection Header serialization
instance Binary ConnectionHeader where
    put = putHeader
    get = getHeader 

-- Prefix a ByteString with its length encoded as a 4-byte little
-- endian integer.
tagLength :: ByteString -> Put
tagLength = putWord32le . fromIntegral . B.length

-- |Serialize a list of key-value pairs using the ROS Connection
-- Header protocol.
putHeader :: ConnectionHeader -> Put
putHeader (ConnectionHeader header) = go header
  where go [] = return () 
        go ((name, value) : xs) = do
            let field = B.pack $ name ++ "=" ++ value
            tagLength     field
            putByteString field
            go xs 

-- Each entry in the header is a 4-byte little endian integer denoting
-- the length of the entry, the field name string, an equals sign, and
-- the field value string.
parsePair :: Get (String, String)
parsePair = do len <- fromIntegral <$> getWord32le
               field <- getByteString len
               case B.elemIndex '=' field of
                 Just i -> return . (unpack *** unpack . B.tail) . B.splitAt i $
                           field
                 Nothing -> error "ConnectionHeader: Didn't find '=' separator in field"

-- Keep parsing header entries until we run out of bytes.
getHeader :: Get ConnectionHeader
getHeader = ConnectionHeader <$> go [] 
  where go acc = do empty <- isEmpty
                    if empty
                      then return acc
                      else go =<< (: acc) <$> parsePair

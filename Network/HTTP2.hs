{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2 (
  -- * Magic
    connectionPreface
  , connectionPrefaceLength
  -- * Encoding and decoding
  , EncodeInfo(..)
  , encodeInfo
  , encodeFrame
  , decodeFrame
  , decodeFrameHeader
  , checkFrameHeader
  , module Network.HTTP2.Types
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

-- | "PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n"
connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

-- | Length of the preface
connectionPrefaceLength :: Int
connectionPrefaceLength = BS.length connectionPreface

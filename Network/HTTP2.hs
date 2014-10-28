{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2 (
  -- * Magic
    connectionPreface
  -- * Encoding and decoding
  , EncodeInfo(..)
  , encodeFrame
  , decodeFrame
  , module Network.HTTP2.Types
  ) where

import Data.ByteString (ByteString)
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

-- | "PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n"
connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

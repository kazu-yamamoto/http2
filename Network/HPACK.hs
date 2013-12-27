module Network.HPACK (
  -- * Type
    ByteStream
  , HeaderSet
  , Context
  , newContext
  , DecodeError(..)
  , HPACKEncoding
  , HPACKDecoding
  -- * Request
  , encodeRequestHeader
  , decodeRequestHeader
  -- * Response
  , encodeResponseHeader
  , decodeResponseHeader
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (throwIO)
import Network.HPACK.Context
import Network.HPACK.HeaderBlock
import Network.HPACK.Huffman
import Network.HPACK.Types

----------------------------------------------------------------

type HPACKEncoding = Context -> HeaderSet  -> IO (Context, ByteStream)
type HPACKDecoding = Context -> ByteStream -> IO (Context, HeaderSet)

----------------------------------------------------------------

-- | Converting 'HeaderSet' for HTTP request to the low level format.
encodeRequestHeader :: HPACKEncoding
encodeRequestHeader ctx hs = second toBS <$> toHeaderBlock ctx hs
  where
    toBS = toByteStream huffmanEncodeInRequest

-- | Converting the low level format for HTTP request to 'HeaderSet'.
--   'DecodeError' would be thrown.
decodeRequestHeader :: HPACKDecoding
decodeRequestHeader ctx bs = either throwIO (fromHeaderBlock ctx) ehb
  where
    ehb = fromByteStream huffmanDecodeInRequest bs

----------------------------------------------------------------

-- | Converting 'HeaderSet' for HTTP response to the low level format.
encodeResponseHeader :: HPACKEncoding
encodeResponseHeader ctx hs = second toBS <$> toHeaderBlock ctx hs
  where
    toBS = toByteStream huffmanEncodeInResponse

-- | Converting the low level format for HTTP response to 'HeaderSet'.
--   'DecodeError' would be thrown.
decodeResponseHeader :: HPACKDecoding
decodeResponseHeader ctx bs = either throwIO (fromHeaderBlock ctx) ehb
  where
    ehb = fromByteStream huffmanDecodeInResponse bs

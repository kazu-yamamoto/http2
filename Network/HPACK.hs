-- | HPACK: encoding and decoding a header list.
module Network.HPACK (
  -- * Encoding and decoding
    HPACKEncoding
  , HPACKDecoding
  , encodeHeader
  , decodeHeader
  -- * HeaderTable
  , HeaderTable
  , newHeaderTableForEncoding
  , newHeaderTableForDecoding
  -- * Strategy for encoding
  , CompressionAlgo(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  -- * Errors for decoding
  , DecodeError(..)
  -- * Headers
  , HeaderList
  , Header
  , HeaderName
  , HeaderValue
  -- * Basic types
  , ByteStream
  , Size
  , Index
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (throwIO)
import Network.HPACK.HeaderBlock (toHeaderBlock, fromHeaderBlock, toByteStream, fromByteStream)
import Network.HPACK.Table (HeaderTable, Size, newHeaderTableForEncoding, newHeaderTableForDecoding)
import Network.HPACK.Types

----------------------------------------------------------------

-- | HPACK encoding, from 'HeaderList' to 'ByteStream'.
type HPACKEncoding = HeaderTable -> HeaderList  -> IO (HeaderTable, ByteStream)

-- | HPACK decoding, from 'ByteStream' to 'HeaderList'.
type HPACKDecoding = HeaderTable -> ByteStream -> IO (HeaderTable, HeaderList)

----------------------------------------------------------------

-- | Converting 'HeaderList' for HTTP request to the low level format.
encodeHeader :: EncodeStrategy -> HPACKEncoding
encodeHeader stgy ctx hs = second toBS <$> toHeaderBlock algo ctx hs
  where
    algo = compressionAlgo stgy
    toBS = toByteStream (useHuffman stgy)

-- | Converting the low level format for HTTP request to 'HeaderList'.
--   'DecodeError' would be thrown.
decodeHeader :: HPACKDecoding
decodeHeader ctx bs = either throwIO (fromHeaderBlock ctx) ehb
  where
    ehb = fromByteStream bs

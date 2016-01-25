{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK2 (
  -- * Encoding and decoding
    HPACKEncoding
  , HPACKDecoding
  , encodeHeader
  , decodeHeader
  -- * Encoding with builders
  , HPACKEncodingBuilder
  , encodeHeaderBuilder
  -- * DynamicTable
  , DynamicTable
  , defaultDynamicTableSize
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , setLimitForEncoding
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
  , Size
  , Index
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Network.HPACK2.HeaderBlock (toHeaderBlock, fromHeaderBlock, toByteString, fromByteString, toBuilder)
import Network.HPACK2.Table (DynamicTable, Size, newDynamicTableForEncoding, newDynamicTableForDecoding, setLimitForEncoding)
import Network.HPACK2.Types

-- | Default dynamic table size.
--   The value is 4,096 bytes: an array has 128 entries.
--
-- >>> defaultDynamicTableSize
-- 4096
defaultDynamicTableSize :: Int
defaultDynamicTableSize = 4096

----------------------------------------------------------------

-- | HPACK encoding from 'HeaderList' to 'ByteString'.
type HPACKEncoding = DynamicTable -> HeaderList -> IO ByteString

-- | HPACK encoding from 'HeaderList' to 'Builder'.
type HPACKEncodingBuilder = DynamicTable -> HeaderList -> IO Builder

-- | HPACK decoding from 'ByteString' to 'HeaderList'.
type HPACKDecoding = DynamicTable -> ByteString -> IO HeaderList

----------------------------------------------------------------

-- | Converting 'HeaderList' for HTTP header to the low level format.
encodeHeader :: EncodeStrategy -> HPACKEncoding
encodeHeader stgy ctx hs = toBS <$> toHeaderBlock algo ctx hs
  where
    algo = compressionAlgo stgy
    toBS = toByteString (useHuffman stgy)

-- | Converting 'HeaderList' for HTTP header to bytestring builder.
encodeHeaderBuilder :: EncodeStrategy -> HPACKEncodingBuilder
encodeHeaderBuilder stgy ctx hs = toBB <$> toHeaderBlock algo ctx hs
  where
    algo = compressionAlgo stgy
    toBB = toBuilder (useHuffman stgy)

-- | Converting the low level format for HTTP header to 'HeaderList'.
--   'DecodeError' would be thrown.
decodeHeader :: HPACKDecoding
decodeHeader ctx bs = either throwIO (fromHeaderBlock ctx) ehb
  where
    ehb = fromByteString bs

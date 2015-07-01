{-# LANGUAGE CPP #-}

-- | HPACK: encoding and decoding a header list.
module Network.HPACK (
  -- * Encoding and decoding
    HPACKEncoding
  , HPACKDecoding
  , encodeHeader
  , encodeHeaderChunks
  , decodeHeader
  -- * DynamicTable
  , DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
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
import Control.Arrow (second)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HPACK.HeaderBlock (toHeaderBlock, fromHeaderBlock, toByteString, toLazyByteString, fromByteString)
import Network.HPACK.Table (DynamicTable, Size, newDynamicTableForEncoding, newDynamicTableForDecoding)
import Network.HPACK.Types

----------------------------------------------------------------

-- | HPACK encoding, from 'HeaderList' to 'ByteString'.
type HPACKEncoding = DynamicTable -> HeaderList  -> IO (DynamicTable, ByteString)

-- | HPACK decoding, from 'ByteString' to 'HeaderList'.
type HPACKDecoding = DynamicTable -> ByteString -> IO (DynamicTable, HeaderList)

----------------------------------------------------------------

-- | Converting 'HeaderList' for HTTP request to the low level format.
encodeHeader :: EncodeStrategy -> HPACKEncoding
encodeHeader stgy ctx hs = second toBS <$> toHeaderBlock algo ctx hs
  where
    algo = compressionAlgo stgy
    toBS = toByteString (useHuffman stgy)

-- | Converting 'HeaderList' for HTTP request to the low level format.
encodeHeaderChunks :: EncodeStrategy -> DynamicTable -> HeaderList -> IO (DynamicTable, [ByteString])
encodeHeaderChunks stgy ctx hs = second (BL.toChunks . toBS) <$> toHeaderBlock algo ctx hs
  where
    algo = compressionAlgo stgy
    toBS = toLazyByteString (useHuffman stgy)

-- | Converting the low level format for HTTP request to 'HeaderList'.
--   'DecodeError' would be thrown.
decodeHeader :: HPACKDecoding
decodeHeader ctx bs = either throwIO (fromHeaderBlock ctx) ehb
  where
    ehb = fromByteString bs

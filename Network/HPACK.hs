{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK (
  -- * Encoding and decoding
    encodeHeader
  , decodeHeader
  -- * Low level
  , HPACKEncodingOne
  , prepareEncodeHeader
  -- * DynamicTable
  , DynamicTable
  , defaultDynamicTableSize
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , clearDynamicTable
  , withDynamicTableForEncoding
  , withDynamicTableForDecoding
  , setLimitForEncoding
  -- * Strategy for encoding
  , CompressionAlgo(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  -- * Errors
  , DecodeError(..)
  , BufferOverrun(..)
  -- * Headers
  , HeaderList
  , Header
  , HeaderName
  , HeaderValue
  -- * Basic types
  , Size
  , Index
  , Buffer
  , BufferSize
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.ByteString (ByteString)
import Network.HPACK.HeaderBlock
import Network.HPACK.Table
import Network.HPACK.Types
import Network.HPACK.Buffer

-- | Default dynamic table size.
--   The value is 4,096 bytes: an array has 128 entries.
--
-- >>> defaultDynamicTableSize
-- 4096
defaultDynamicTableSize :: Int
defaultDynamicTableSize = 4096

----------------------------------------------------------------

-- | Converting 'HeaderList' to the HPACK format.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader :: EncodeStrategy
             -> Size -- ^ The size of a temporary buffer.
             -> DynamicTable
             -> HeaderList
             -> IO ByteString -- ^ An HPACK format
encodeHeader stgy siz dyntbl hs0 = withTemporaryBuffer siz $ \wbuf -> do
    encodeHeaderOne <- prepareEncodeHeader stgy dyntbl wbuf
    go encodeHeaderOne wbuf hs0
  where
    go _   _    []     = return ()
    go enc wbuf (h:hs) = do
        _ <- enc dyntbl wbuf h -- fixme: why?
        go enc wbuf hs

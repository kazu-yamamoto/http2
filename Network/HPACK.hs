{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK (
  -- * Encoding
    encodeHeader
  , HPACKEncodingOne
  , prepareEncodeHeader
  -- * Decoding
  , decodeHeader
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
  , Buffer
  , BufferSize
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.ByteString (ByteString)
import Network.HPACK.HeaderBlock (decodeHeader, HPACKEncodingOne, prepareEncodeHeader)
import Network.HPACK.Table (DynamicTable, Size, newDynamicTableForEncoding, newDynamicTableForDecoding, setLimitForEncoding)
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

-- | Converting 'HeaderList' for HTTP header to the low level format.
encodeHeader :: EncodeStrategy -> DynamicTable -> HeaderList -> IO ByteString
encodeHeader stgy dyntbl hs0 = withTemporaryBuffer 4096 $ \wbuf -> do
    encodeHeaderOne <- prepareEncodeHeader stgy dyntbl wbuf
    go encodeHeaderOne wbuf hs0
  where
    go _   _    []     = return ()
    go enc wbuf (h:hs) = do
        _ <- enc dyntbl wbuf h -- fixme: why?
        go enc wbuf hs

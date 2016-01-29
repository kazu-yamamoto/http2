{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK2 (
  -- * Encoding
    HPACKEncoding
  , encodeHeader
  , HPACKEncodingOne
  , prepareEncodeHeader
  -- * Decoding
  , HPACKDecoding
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
import Network.HPACK2.HeaderBlock (HPACKDecoding, decodeHeader, HPACKEncodingOne, prepareEncodeHeader)
import Network.HPACK2.Table (DynamicTable, Size, newDynamicTableForEncoding, newDynamicTableForDecoding, setLimitForEncoding)
import Network.HPACK2.Types
import Network.HPACK2.Buffer

import Foreign.Marshal.Alloc
import Control.Exception

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

----------------------------------------------------------------

-- | Converting 'HeaderList' for HTTP header to the low level format.
encodeHeader :: EncodeStrategy -> HPACKEncoding
encodeHeader stgy dyntbl hs0 = bracket (mallocBytes 4096) free $ \buf -> do
    wbuf <- newWorkingBuffer buf 4096
    encodeHeaderOne <- prepareEncodeHeader stgy dyntbl wbuf
    go encodeHeaderOne wbuf hs0
    toByteString wbuf
  where
    go _   _    []     = return ()
    go enc wbuf (h:hs) = do
        _ <- enc dyntbl wbuf h -- fixme: why?
        go enc wbuf hs

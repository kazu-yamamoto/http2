{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK (
    -- * Encoding and decoding
    encodeHeader,
    decodeHeader,
    Header,
    original,
    foldedCase,
    mk,

    -- * Encoding and decoding with token
    encodeTokenHeader,
    decodeTokenHeader,

    -- * DynamicTable
    DynamicTable,
    defaultDynamicTableSize,
    newDynamicTableForEncoding,
    newDynamicTableForDecoding,
    withDynamicTableForEncoding,
    withDynamicTableForDecoding,
    setLimitForEncoding,

    -- * Strategy for encoding
    CompressionAlgo (..),
    EncodeStrategy (..),
    defaultEncodeStrategy,

    -- * Errors
    DecodeError (..),
    BufferOverrun (..),

    -- * Token header
    FieldValue,
    TokenHeader,
    TokenHeaderList,
    toTokenHeaderTable,

    -- * Value table
    ValueTable,
    TokenHeaderTable,
    getFieldValue,

    -- * Basic types
    Size,
    Index,
    Buffer,
    BufferSize,
) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif

import Imports
import Network.HPACK.HeaderBlock
import Network.HPACK.Table
import Network.HPACK.Types

-- | Default dynamic table size.
--   The value is 4,096 bytes: an array has 128 entries.
--
-- >>> defaultDynamicTableSize
-- 4096
defaultDynamicTableSize :: Int
defaultDynamicTableSize = 4096

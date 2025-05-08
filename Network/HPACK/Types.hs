{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
    -- * Header
    FieldValue,
    TokenHeader,
    TokenHeaderList,

    -- * Misc
    Index,
    HIndex (..),

    -- * Encoding and decoding
    CompressionAlgo (..),
    EncodeStrategy (..),
    defaultEncodeStrategy,
    DecodeError (..),

    -- * Buffer
    Buffer,
    BufferSize,
    BufferOverrun (..),
) where

import Control.Exception as E
import Network.ByteOrder (Buffer, BufferOverrun (..), BufferSize)

import Imports

----------------------------------------------------------------

-- | Index for table.
type Index = Int

data HIndex = SIndex Int | DIndex Int deriving (Eq, Ord, Show)

----------------------------------------------------------------

-- | Compression algorithms for HPACK encoding.
data CompressionAlgo
    = -- | No compression
      Naive
    | -- | Using indices in the static table only
      Static
    | -- | Using indices
      Linear
    deriving (Eq, Show)

-- | Strategy for HPACK encoding.
data EncodeStrategy = EncodeStrategy
    { compressionAlgo :: CompressionAlgo
    -- ^ Which compression algorithm is used.
    , useHuffman :: Bool
    -- ^ Whether or not to use Huffman encoding for strings.
    }
    deriving (Eq, Show)

-- | Default 'EncodeStrategy'.
--
-- >>> defaultEncodeStrategy
-- EncodeStrategy {compressionAlgo = Linear, useHuffman = False}
defaultEncodeStrategy :: EncodeStrategy
defaultEncodeStrategy =
    EncodeStrategy
        { compressionAlgo = Linear
        , useHuffman = False
        }

----------------------------------------------------------------

-- | Errors for decoder.
data DecodeError
    = -- | Index is out of range
      IndexOverrun Index
    | -- | Eos appears in the middle of huffman string
      EosInTheMiddle
    | -- | Non-eos appears in the end of huffman string
      IllegalEos
    | -- | Eos of huffman string is more than 7 bits
      TooLongEos
    | -- | A peer set the dynamic table size less than 32
      TooSmallTableSize
    | -- | A peer tried to change the dynamic table size over the limit
      TooLargeTableSize
    | -- | Table size update at the non-beginning
      IllegalTableSizeUpdate
    | HeaderBlockTruncated
    | IllegalHeaderName
    | TooLargeHeader
    deriving (Eq, Show)

instance Exception DecodeError

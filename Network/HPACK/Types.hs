{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , Header
  , HeaderList
  , TokenHeader
  , TokenHeaderList
  -- * Misc
  , Index
  , HIndex(..)
  -- * Encoding and decoding
  , CompressionAlgo(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  , DecodeError(..)
  -- * Buffer
  , Buffer
  , BufferSize
  , BufferOverrun(..)
  ) where

import Control.Exception as E
import Data.Typeable
import Network.ByteOrder (Buffer, BufferSize, BufferOverrun(..))

import Imports
import Network.HPACK.Token (Token)

----------------------------------------------------------------

-- | Header name.
type HeaderName = ByteString

-- | Header value.
type HeaderValue = ByteString

-- | Header.
type Header = (HeaderName, HeaderValue)

-- | Header list.
type HeaderList = [Header]

-- | TokenBased header.
type TokenHeader = (Token, HeaderValue)

-- | TokenBased header list.
type TokenHeaderList = [TokenHeader]

----------------------------------------------------------------

-- | Index for table.
type Index = Int

data HIndex = SIndex Int | DIndex Int deriving (Eq, Ord, Show)

----------------------------------------------------------------

-- | Compression algorithms for HPACK encoding.
data CompressionAlgo = Naive  -- ^ No compression
                     | Static -- ^ Using indices in the static table only
                     | Linear -- ^ Using indices
                     deriving (Eq, Show)

-- | Strategy for HPACK encoding.
data EncodeStrategy = EncodeStrategy {
  -- | Which compression algorithm is used.
    compressionAlgo :: CompressionAlgo
  -- | Whether or not to use Huffman encoding for strings.
  , useHuffman :: Bool
  } deriving (Eq, Show)

-- | Default 'EncodeStrategy'.
--
-- >>> defaultEncodeStrategy
-- EncodeStrategy {compressionAlgo = Linear, useHuffman = False}
defaultEncodeStrategy :: EncodeStrategy
defaultEncodeStrategy = EncodeStrategy {
    compressionAlgo = Linear
  , useHuffman = False
  }

----------------------------------------------------------------

-- | Errors for decoder.
data DecodeError = IndexOverrun Index -- ^ Index is out of range
                 | EosInTheMiddle -- ^ Eos appears in the middle of huffman string
                 | IllegalEos -- ^ Non-eos appears in the end of huffman string
                 | TooLongEos -- ^ Eos of huffman string is more than 7 bits
                 | TooSmallTableSize -- ^ A peer set the dynamic table size less than 32
                 | TooLargeTableSize -- ^ A peer tried to change the dynamic table size over the limit
                 | IllegalTableSizeUpdate -- ^ Table size update at the non-beginning
                 | HeaderBlockTruncated
                 | IllegalHeaderName
                 | TooLargeHeader
                 deriving (Eq,Show,Typeable)

instance Exception DecodeError

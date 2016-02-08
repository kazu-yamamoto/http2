{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK2.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , HeaderStuff
  , Header
  , HeaderList
  -- * Misc
  , Index
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
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

----------------------------------------------------------------

-- | Header name.
type HeaderName = ByteString

-- | Header value.
type HeaderValue = ByteString

-- | Header.
type Header = (HeaderName, HeaderValue)

-- | Header list.
type HeaderList = [Header]

-- | To be a 'HeaderName' or 'HeaderValue'.
type HeaderStuff = ByteString

-- | Index for table.
type Index = Int

----------------------------------------------------------------

-- | Compression algorithms for HPACK encoding.
data CompressionAlgo = Naive  -- ^ No compression
                     | Static -- ^ Using the static table only
                     | Linear -- ^ Using indices only
                     deriving (Eq, Show)

-- | Strategy for HPACK encoding.
data EncodeStrategy = EncodeStrategy {
  -- | Which compression algorithm is used.
    compressionAlgo :: !CompressionAlgo
  -- | Whether or not to use Huffman encoding for strings.
  , useHuffman :: !Bool
  } deriving (Eq, Show)

-- | Default 'EncodeStrategy'.
--
-- >>> defaultEncodeStrategy
-- EncodeStrategy {compressionAlgo = Linear, useHuffman = True}
defaultEncodeStrategy :: EncodeStrategy
defaultEncodeStrategy = EncodeStrategy {
    compressionAlgo = Linear
  , useHuffman = True
  }

----------------------------------------------------------------

-- | Errors for decoder.
data DecodeError = IndexOverrun Index -- ^ Index is out of range
                 | EosInTheMiddle -- ^ Eos appears in the middle of huffman string
                 | IllegalEos -- ^ Non-eos appears in the end of huffman string
                 | TooLongEos -- ^ Eos of huffman string is more than 7 bits
                 | EmptyEncodedString -- ^ Encoded string has no length
                 | TooLargeTableSize -- ^ A peer tried to change the dynamic table size over the limit (or sometime less than 32)
                 | IllegalTableSizeUpdate -- ^ Table size update at the non-beginning
                 | HeaderBlockTruncated
                 deriving (Eq,Show,Typeable)

instance Exception DecodeError

----------------------------------------------------------------

-- | Buffer type.
type Buffer = Ptr Word8

-- | The size of buffer.
type BufferSize = Int

data BufferOverrun = BufferOverrun -- ^ The buffer size is not enough
                     deriving (Eq,Show,Typeable)

instance Exception BufferOverrun

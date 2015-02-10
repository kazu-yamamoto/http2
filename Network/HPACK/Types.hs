{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
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
  ) where

import Control.Exception as E
import Data.ByteString (ByteString)
import Data.Typeable

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

-- | Compression algorithms for HPACK encoding.
data CompressionAlgo = Naive  -- ^ No compression
                     | Static -- ^ Using the static table only
                     | Linear -- ^ Using indices only

-- | Strategy for HPACK encoding.
data EncodeStrategy = EncodeStrategy {
  -- | Which compression algorithm is used.
    compressionAlgo :: CompressionAlgo
  -- | Whether or not to use Huffman encoding for strings.
  , useHuffman :: Bool
  }

-- | Default 'EncodeStrategy'. 'compressionAlgo' is 'Linear' and 'useHuffman' is 'True'.
defaultEncodeStrategy :: EncodeStrategy
defaultEncodeStrategy = EncodeStrategy {
    compressionAlgo = Linear
  , useHuffman = True
  }


-- | Errors for decoder.
data DecodeError = IndexOverrun Index -- ^ Index is out of range
                 | EosInTheMiddle -- ^ Eos appears in the middle of huffman string
                 | IllegalEos -- ^ Non-eos appears in the end of huffman string
                 | TooLongEos -- ^ Eos of huffman string is more than 7 bits
                 | EmptyEncodedString -- ^ Encoded string has no length
                 | EmptyBlock -- ^ Header block is empty
                 deriving (Eq,Show,Typeable)

instance Exception DecodeError

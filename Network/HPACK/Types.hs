{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , HeaderStuff
  , Header
  -- * Misc
  , ByteStream
  , Index
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

-- | To be a 'HeaderName' or 'HeaderValue'.
type HeaderStuff = ByteString

-- | Byte stream in HTTP request/response.
type ByteStream = ByteString

-- | Index for table.
type Index = Int

-- | Errors for decoder.
data DecodeError = IndexOverrun Index -- ^ Index is out of range
                 | EosInTheMiddle -- ^ Eos appears in the middle of huffman string
                 | IllegalEos -- ^ Non-eos appears in the end of huffman string
                 | TooLongEos -- ^ Eos of huffman string is more than 7 bits
                 | EmptyEncodedString -- ^ Encoded string has no length
                 | EmptyBlock -- ^ Header block is empty
                 deriving (Eq,Show,Typeable)

instance Exception DecodeError

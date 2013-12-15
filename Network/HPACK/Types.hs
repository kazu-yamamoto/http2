{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , HeaderStuff
  , fromHeaderName
  , toHeaderName
  , Header
  -- * Misc
  , ByteStream
  , Index
  , DecodeError(..)
  ) where

import Control.Exception as E
import Data.ByteString (ByteString)
import Data.CaseInsensitive (foldedCase, mk)
import Data.Typeable
import Network.HTTP.Types (HeaderName, Header)

-- | Header value.
type HeaderValue = ByteString

-- | To be a 'HeaderName' or 'HeaderValue'.
type HeaderStuff = ByteString

-- | Converting 'HeaderName' to 'HeaderStuff'.
fromHeaderName :: HeaderName -> HeaderStuff
fromHeaderName = foldedCase

-- | Converting 'HeaderStuff' to 'HeaderName'.
toHeaderName :: HeaderStuff -> HeaderName
toHeaderName = mk

-- | Byte stream in HTTP request/response.
type ByteStream = ByteString

-- | Index for table.
type Index = Int

-- | Errors for decoder.
data DecodeError = IndexOverrun Index -- ^ Index is out of range
                 deriving (Show,Typeable)

instance Exception DecodeError

{-# LANGUAGE DeriveDataTypeable #-}

module Network.HPACK.Types (
    HeaderName
  , HeaderValue
  , HeaderStuff
  , Header
  , ByteStream
  , Index
  , DecodeError(..)
  ) where

import Control.Exception as E
import Data.Typeable
import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName, Header)

-- | Header value.
type HeaderValue = ByteString

-- | To be a 'HeaderName' or 'HeaderValue'.
type HeaderStuff = ByteString

-- | Byte stream in HTTP request/response.
type ByteStream = ByteString

-- | Index for table.
type Index = Int

-- | Errors for decoder.
data DecodeError = IndexOverrun -- ^ Index is out of range
                 deriving (Show,Typeable)

instance Exception DecodeError

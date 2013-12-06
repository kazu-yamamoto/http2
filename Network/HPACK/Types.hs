module Network.HPACK.Types (
    HeaderName
  , HeaderValue
  , HeaderStuff
  , Header
  , Index
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName, Header)

-- | Header value.
type HeaderValue = ByteString

-- | To be a 'HeaderName' or 'HeaderValue'.
type HeaderStuff = ByteString

-- | Index for table.
type Index = Int

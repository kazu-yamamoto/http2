module Network.HPACK.Types (
    HeaderName
  , HeaderValue
  , Header
  , Index
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName, Header)

-- | Header value
type HeaderValue = ByteString

-- | Index for table.
type Index = Int

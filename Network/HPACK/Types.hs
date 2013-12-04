module Network.HPACK.Types (
  -- * Header
    HeaderName
  , HeaderValue
  , Header
  , HeaderSet
  -- * Representation
  , HeaderBlock
  , Representation(..)
  , Index
  , Indexing(..)
  , Naming(..)
  ) where

import Data.ByteString (ByteString)

----------------------------------------------------------------

-- | Header name (FIXME).
type HeaderName = ByteString
-- | Header value (FIXME).
type HeaderValue = ByteString
-- | Header (FIXME)
type Header = (HeaderName, HeaderValue)
-- | Header set
type HeaderSet = [Header]

----------------------------------------------------------------

-- | Type for header block.
type HeaderBlock = [Representation]

-- | Type for representation.
data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue
                    deriving Show

-- | Index for table.
type Index = Int

-- | Whether or not adding to a table.
data Indexing = Add | NotAdd deriving Show

-- | Index or literal.
data Naming = Idx Index | Lit HeaderName deriving Show

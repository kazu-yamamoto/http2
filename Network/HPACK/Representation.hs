module Network.HPACK.Representation (
  -- * Type
    HeaderBlock
  , Representation(..)
  , HeaderName  -- re-exporting
  , HeaderValue -- re-exporting
  , Index       -- re-exporting
  , Indexing(..)
  , Naming(..)
  ) where

import Network.HTTP.Types (HeaderName)
import Network.HPACK.Table (HeaderValue, Index)

----------------------------------------------------------------

-- | Type for header block.
type HeaderBlock = [Representation]

-- | Type for representation.
data Representation = Indexed Index
                    | Literal Indexing Naming HeaderValue
                    deriving Show

-- | Whether or not adding to a table.
data Indexing = Add | NotAdd deriving Show

-- | Index or literal.
data Naming = Idx Index | Lit HeaderName deriving Show

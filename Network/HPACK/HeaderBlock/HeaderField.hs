module Network.HPACK.HeaderBlock.HeaderField (
  -- * Type
    HeaderBlock
  , emptyHeaderBlock
  , HeaderField(..)
  , HeaderName  -- re-exporting
  , HeaderValue -- re-exporting
  , Index       -- re-exporting
  , Indexing(..)
  , Naming(..)
  ) where

import Network.HPACK.Types

----------------------------------------------------------------

-- | Type for header block.
type HeaderBlock = [HeaderField]

-- | Empty header block.
emptyHeaderBlock :: HeaderBlock
emptyHeaderBlock = []

-- | Type for representation.
data HeaderField = ChangeTableSize Int
                 | Indexed Index
                 | Literal Indexing Naming HeaderValue
                 deriving (Eq,Show)

-- | Whether or not adding to a table.
data Indexing = Add | NotAdd | Never deriving (Eq,Show)

-- | Index or literal.
data Naming = Idx Index | Lit HeaderName deriving (Eq,Show)

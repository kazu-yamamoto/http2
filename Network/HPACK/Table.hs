module Network.HPACK.Table (
  -- * Header table
    Index
  , HeaderTable
  , newHeaderTable
  , showHeaderTable
  , insertEntry
  -- * Entry
  , module Network.HPACK.Table.Entry
  -- * Which tables
  , WhichTable(..)
  , which
  , fromWhich
  ) where

import Data.Array ((!))
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Table.Types

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable Entry
                | InStaticTable Entry
                | IndexError
                deriving Eq

(.!.) :: HeaderTable -> Index -> WhichTable
HeaderTable maxN off n tbl _ _ .!. idx
  | idx <= n                        = InHeaderTable $ tbl ! pidx
  | 1 <= stcidx && stcidx <= stcsiz = InStaticTable $ stctbl ! stcidx
  | otherwise                       = IndexError
  where
    StaticTable stcsiz stctbl = staticTable
    stcidx = idx - n
    pidx = (off + idx + maxN) `mod` maxN

----------------------------------------------------------------

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> WhichTable
which hdrtbl idx = hdrtbl .!. idx

-- | Extracting 'Entry'.
fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e
fromWhich _                 = error "fromWhich"

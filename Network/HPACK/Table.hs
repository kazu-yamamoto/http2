module Network.HPACK.Table (
    HeaderTable
  , newHeaderTable
  , insertEntry
  , WhichTable(..)
  , which
  , fromWhich
  ) where

import Data.Array ((!))
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Types

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

which :: HeaderTable -> Index -> WhichTable
which hdrtbl idx = hdrtbl .!. idx

fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e
fromWhich _                 = error "fromWhich"

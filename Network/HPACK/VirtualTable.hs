module Network.HPACK.VirtualTable (
    WhichTable(..)
  , whichTable
  , notEmittedHeaders
  ) where

import Data.Array ((!))
import Data.List (partition)
import Network.HPACK.Context
import Network.HPACK.Entry
import Network.HPACK.HeaderTable
import Network.HPACK.ReferenceSet
import Network.HPACK.StaticTable
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

-- | Detecting which table does `Index` refer to?
whichTable :: Index -> Context -> WhichTable
whichTable idx ctx = hdrtbl .!. idx
  where
    hdrtbl = headerTable ctx

----------------------------------------------------------------

-- | Obtaining non-emitted headers.
notEmittedHeaders :: Context -> Maybe [Header]
notEmittedHeaders ctx
  | null ls   = Just xs
  | otherwise = Nothing
  where
    ReferenceSet is = oldReferenceSet ctx
    ws = map (\i -> hdrtbl .!. i) is
    (ls,rs) = partition (== IndexError) ws
    hdrtbl = headerTable ctx
    xs = map (fromEntry . fromWhich) rs

fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e
fromWhich _                 = error "fromWhich"

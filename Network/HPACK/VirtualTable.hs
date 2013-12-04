module Network.HPACK.VirtualTable (
    WhichTable(..)
  , whichTable
  , allEnts
  ) where

import Data.Array ((!))
import Data.List (partition)
import Network.HPACK.Context
import Network.HPACK.HeaderTable
import Network.HPACK.ReferenceSet
import Network.HPACK.StaticTable
import Network.HPACK.Types

----------------------------------------------------------------

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

whichTable :: Index -> Context -> WhichTable
whichTable idx ctx = hdrtbl .!. idx
  where
    hdrtbl = headerTable ctx

allEnts :: Context -> ([WhichTable], [WhichTable])
allEnts ctx = partition (== IndexError) ws
  where
    ReferenceSet is = oldReferenceSet ctx
    hdrtbl = headerTable ctx
    ws = map (\i -> hdrtbl .!. i) is

module Network.HPACK.Table (
    getEntry
  , notEmittedEntries
  , switchAction
  ) where

import Data.Array ((!))
import Data.List (partition)
import Network.HPACK.Context
import Network.HPACK.ReferenceSet
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

-- | Detecting which table does `Index` refer to?
whichTable :: Index -> Context -> WhichTable
whichTable idx ctx = which hdrtbl idx
  where
    hdrtbl = headerTable ctx

----------------------------------------------------------------

-- | Getting 'Entry' by 'Index'.
getEntry :: Index -> Context -> Maybe Entry
getEntry idx ctx = case whichTable idx ctx of
    InHeaderTable e -> Just e
    InStaticTable e -> Just e
    IndexError      -> Nothing

----------------------------------------------------------------

-- | Obtaining non-emitted entries.
notEmittedEntries :: Context -> Maybe [Entry]
notEmittedEntries ctx
  | null ls   = Just xs
  | otherwise = Nothing
  where
    is = getIndices $ oldReferenceSet ctx
    hdrtbl = headerTable ctx
    ws = map (which hdrtbl) is
    (ls,rs) = partition (== IndexError) ws
    xs = map fromWhich rs

fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e
fromWhich _                 = error "fromWhich"

----------------------------------------------------------------

-- | Choosing an action depending on which tables.
switchAction :: Context -> Index
             -> (Entry -> Context) -- ^ An action for static table
             -> (Entry -> Context) -- ^ An action for header table
             -> Maybe Context
switchAction ctx idx actionForStatic actionForHeaderTable = case whichTable idx ctx of
    IndexError      -> Nothing
    InStaticTable e -> Just $ actionForStatic e
    InHeaderTable e -> Just $ actionForHeaderTable e

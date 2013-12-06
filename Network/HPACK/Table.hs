module Network.HPACK.Table (
  -- * Header table
    HeaderTable
  , newHeaderTable
  , printHeaderTable
  , insertEntry
  -- * Entry
  , module Network.HPACK.Table.Entry
  -- * Which tables
  , WhichTable(..)
  , which
  , fromWhich
  ) where

import Control.Exception (throwIO)
import Data.Array ((!))
import Data.Array.IO (readArray)
import Control.Applicative ((<$>))
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable Entry
                | InStaticTable Entry
                deriving Eq

----------------------------------------------------------------

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> IO WhichTable
which (HeaderTable maxN off n tbl _ _) idx
  | idx <= n                        = InHeaderTable <$> readArray tbl pidx
  | 1 <= stcidx && stcidx <= stcsiz = return $ InStaticTable $ stctbl ! stcidx
  | otherwise                       = throwIO IndexOverrun
  where
    StaticTable stcsiz stctbl = staticTable
    stcidx = idx - n
    pidx = (off + idx + maxN) `mod` maxN

-- | Extracting 'Entry'.
fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e

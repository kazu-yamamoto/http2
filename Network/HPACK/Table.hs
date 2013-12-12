module Network.HPACK.Table (
  -- * Header table
    HeaderTable
  , newHeaderTable
  , printHeaderTable
  -- * Insertion
  , insertEntry
  -- * Lookup
  , HeaderCache(..)
  , lookupTable
  -- * Entry
  , module Network.HPACK.Table.Entry
  -- * Which tables
  , WhichTable(..)
  , which
  , fromWhich
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.Array.IO (readArray)
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

data HeaderCache = None | KeyOnly Index | KeyValue Index

-- | Looking up the static table and the header table.
lookupTable :: Header -> HeaderTable -> IO HeaderCache
lookupTable h hdrtbl
  | isColon h = do
      mi <- toStaticColonIndex h
      case mi of
          Just i  -> return $ KeyValue i
          Nothing -> lookupHeaderTable h hdrtbl
  | otherwise = lookupHeaderTable h hdrtbl

lookupHeaderTable :: Header -> HeaderTable -> IO HeaderCache
lookupHeaderTable (k,v) hdrtbl = do
    miv <- toIndexValue k hdrtbl
    return $ case miv of
        Nothing       -> None
        Just (i,val)
          | val == v  -> KeyValue i
          | otherwise -> KeyOnly i

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
  | 1 <= stcidx && stcidx <= stcsiz = return $ InStaticTable $ toStaticEntry stcidx
  | otherwise                       = throwIO IndexOverrun
  where
    stcsiz = staticTableSize
    stcidx = idx - n
    pidx = (off + idx + maxN) `mod` maxN

-- | Extracting 'Entry'.
fromWhich :: WhichTable -> Entry
fromWhich (InHeaderTable e) = e
fromWhich (InStaticTable e) = e

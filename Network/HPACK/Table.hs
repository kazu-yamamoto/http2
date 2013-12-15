{-# LANGUAGE TupleSections #-}

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
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.Array.IO (readArray)
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

data HeaderCache = None | KeyOnly WhichTable Index | KeyValue WhichTable Index

-- | Looking up the static table and the header table.
lookupTable :: Header -> HeaderTable -> IO HeaderCache
lookupTable h@(k,v) hdrtbl = do
    miv <- toIndexValue k hdrtbl
    case miv of
        Just (i,val)
          | val == v  -> return $ KeyValue (whic i hdrtbl) i
          | otherwise -> return $ KeyOnly (whic i hdrtbl) i
        Nothing
          | isColon h -> do
              mi <- toStaticColonIndex h
              case mi of
                  Just i  -> return $ KeyValue InStaticTable (i + numOfEntries hdrtbl)
                  Nothing -> return $ None
          | otherwise -> return $ None

whic :: Int -> HeaderTable -> WhichTable
whic i hdrtbl
 | i <= numOfEntries hdrtbl = InHeaderTable
 | otherwise                = InStaticTable

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable | InStaticTable deriving Eq

----------------------------------------------------------------

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> IO (WhichTable, Entry)
which (HeaderTable maxN off n tbl _ _) idx
  | idx <= n                        = (InHeaderTable,) <$> readArray tbl pidx
  | 1 <= stcidx && stcidx <= stcsiz = return (InStaticTable, toStaticEntry stcidx)
  | otherwise                       = throwIO $ IndexOverrun idx
  where
    stcsiz = staticTableSize
    stcidx = idx - n
    pidx = (off + idx + maxN) `mod` maxN

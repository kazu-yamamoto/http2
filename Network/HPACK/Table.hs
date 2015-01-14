{-# LANGUAGE TupleSections, RecordWildCards #-}

module Network.HPACK.Table (
  -- * dynamic table
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , renewDynamicTable
  , printDynamicTable
  , isDynamicTableEmpty
  -- * Insertion
  , insertEntry
  -- * Header to index
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
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.Entry
import qualified Network.HPACK.Table.HashPSQ as HP
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InDynamicTable | InStaticTable deriving (Eq,Show)

-- | Is header key-value stored in the tables?
data HeaderCache = None
                 | KeyOnly WhichTable Index
                 | KeyValue WhichTable Index deriving Show

----------------------------------------------------------------

-- | Resolving an index from a header.
--   Static table is prefer to dynamic table.
lookupTable :: Header -> DynamicTable -> HeaderCache
lookupTable h hdrtbl = case reverseIndex hdrtbl of
    Nothing            -> None
    Just rev -> case HP.search h rev of
        HP.N       -> case mstatic of
            HP.N       -> None
            HP.K  sidx -> KeyOnly  InStaticTable (fromSIndexToIndex hdrtbl sidx)
            HP.KV sidx -> KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
        HP.K hidx  -> case mstatic of
            HP.N       -> KeyOnly  InDynamicTable (fromHIndexToIndex hdrtbl hidx)
            HP.K  sidx -> KeyOnly  InStaticTable (fromSIndexToIndex hdrtbl sidx)
            HP.KV sidx -> KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
        HP.KV hidx -> case mstatic of
            HP.N       -> KeyValue InDynamicTable (fromHIndexToIndex hdrtbl hidx)
            HP.K  _    -> KeyValue InDynamicTable (fromHIndexToIndex hdrtbl hidx)
            HP.KV sidx -> KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
  where
    mstatic = HP.search h staticHashPSQ

----------------------------------------------------------------

isIn :: Int -> DynamicTable -> Bool
isIn idx DynamicTable{..} = idx > staticTableSize

-- | Which table does 'Index' belong to?
which :: DynamicTable -> Index -> IO (WhichTable, Entry)
which hdrtbl idx
  | idx `isIn` hdrtbl  = (InDynamicTable,) <$> toHeaderEntry hdrtbl hidx
  | isSIndexValid sidx = return (InStaticTable, toStaticEntry sidx)
  | otherwise          = throwIO $ IndexOverrun idx
  where
    hidx = fromIndexToHIndex hdrtbl idx
    sidx = fromIndexToSIndex hdrtbl idx

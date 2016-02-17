{-# LANGUAGE TupleSections, RecordWildCards, CPP #-}

module Network.HPACK.Table (
  -- * dynamic table
    DynamicTable(..)
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , clearDynamicTable
  , withDynamicTableForEncoding
  , withDynamicTableForDecoding
  , huffmanDecoder
  , renewDynamicTable
  , printDynamicTable
  , isDynamicTableEmpty
  , isSuitableSize
  , TableSizeAction(..)
  , needChangeTableSize
  , setLimitForEncoding
  , resetLimitForEncoding
  -- * Insertion
  , insertEntry
  -- * Entry
  , module Network.HPACK.Table.Entry
  -- * Index to entry
  , CodeInfo(..)
  , toIndexedEntry
  , fromSIndexToIndex
  , fromDIndexToIndex
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

{-# INLINE toIndexedEntry #-}
toIndexedEntry :: DynamicTable -> Index -> IO Entry
toIndexedEntry dyntbl idx
  | idx <= 0               = throwIO $ IndexOverrun idx
  | idx <= staticTableSize = return $! toStaticEntry idx
  | otherwise              = toDynamicEntry dyntbl idx

{-# INLINE fromSIndexToIndex #-}
fromSIndexToIndex :: SIndex -> Index
fromSIndexToIndex (SIndex idx) = idx

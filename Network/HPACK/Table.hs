{-# LANGUAGE TupleSections, CPP #-}

module Network.HPACK.Table (
  -- * dynamic table
    DynamicTable
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
  , module Network.HPACK.Table.RevIndex
  -- * Index to entry
  , toIndexedEntry
  , fromHIndexToIndex
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Network.HPACK.Table.Dynamic
import Network.HPACK.Table.Entry
import Network.HPACK.Table.RevIndex

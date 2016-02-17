{-# LANGUAGE TupleSections, RecordWildCards, CPP #-}

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
  -- * Which tables
  , which
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

-- | Which table does `Index` refer to?
{-# INLINE isIn #-}
isIn :: Int -> DynamicTable -> Bool
isIn idx DynamicTable{..} = idx > staticTableSize

-- | Which table does 'Index' belong to?
which :: DynamicTable -> Index -> IO Entry
which dyntbl idx
  | idx `isIn` dyntbl  = fromIndexToDIndex dyntbl idx >>= toHeaderEntry dyntbl
  | isSIndexValid sidx = return $! toStaticEntry sidx
  | otherwise          = throwIO $ IndexOverrun idx
  where
    sidx = fromIndexToSIndex idx

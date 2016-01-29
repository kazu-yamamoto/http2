{-# LANGUAGE TupleSections, RecordWildCards, CPP #-}

module Network.HPACK2.Table (
  -- * dynamic table
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
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
  -- * Header to index
  , HeaderCache(..)
  , lookupTable
  -- * Entry
  , module Network.HPACK2.Table.Entry
  -- * Which tables
  , WhichTable(..)
  , which
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import qualified Data.Map as M
import Network.HPACK2.Table.Dynamic
import Network.HPACK2.Table.Entry
import Network.HPACK2.Table.RevIndex
import Network.HPACK2.Table.Static
import Network.HPACK2.Types
import Data.IORef (readIORef)

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InDynamicTable | InStaticTable deriving (Eq,Show)

-- | Is header key-value stored in the tables?
data HeaderCache = None
                 | KeyOnly  !WhichTable !Index
                 | KeyValue !WhichTable !Index
                 deriving Show

----------------------------------------------------------------

-- | Resolving an index from a header.
--   Static table is prefer to dynamic table.
lookupTable :: Header -> DynamicTable -> IO HeaderCache
lookupTable (k,v) dyntbl@DynamicTable{..} = do
    mrev <- readIORef reverseIndex
    case mrev of
        Nothing -> return None
        Just (Outer rev) -> case M.lookup k rev of
            Nothing -> return None
            Just (Inner ss ds) -> case lookup v ss of
                Just sidx -> return $! KeyValue InStaticTable (fromSIndexToIndex sidx)
                Nothing   -> case lookup v ds of
                    Just didx -> KeyValue InDynamicTable <$> fromDIndexToIndex dyntbl didx
                    Nothing -> case ss of
                        ((_,sidx):_) -> return $! KeyOnly InStaticTable (fromSIndexToIndex sidx)
                        [] -> case ds of
                            ((_,didx):_) -> KeyOnly InDynamicTable <$> fromDIndexToIndex dyntbl didx
                            _ -> error "search"

----------------------------------------------------------------

{-# INLINE isIn #-}
isIn :: Int -> DynamicTable -> Bool
isIn idx DynamicTable{..} = idx > staticTableSize

-- | Which table does 'Index' belong to?
which :: DynamicTable -> Index -> IO (WhichTable, Entry)
which dyntbl idx
  | idx `isIn` dyntbl  = do
        hidx <- fromIndexToDIndex dyntbl idx
        (InDynamicTable,) <$> toHeaderEntry dyntbl hidx
  | isSIndexValid sidx = return (InStaticTable, toStaticEntry sidx)
  | otherwise          = throwIO $ IndexOverrun idx
  where
    sidx = fromIndexToSIndex idx

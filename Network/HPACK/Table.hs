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
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Header
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

data HeaderCache = None | KeyOnly WhichTable Index | KeyValue WhichTable Index deriving Show

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
                  Just sidx -> return $ KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
                  Nothing -> do
                      mj <- toStaticIndex k
                      case mj of
                          Just sidx -> return $ KeyOnly InStaticTable (fromSIndexToIndex hdrtbl sidx)
                          Nothing -> return $ None
          | otherwise -> do
              mj <- toStaticIndex k
              case mj of
                  Just sidx -> return $ KeyOnly InStaticTable (fromSIndexToIndex hdrtbl sidx)
                  Nothing -> return $ None


whic :: Int -> HeaderTable -> WhichTable
whic idx hdrtbl
 | idx `isIn` hdrtbl = InHeaderTable
 | otherwise         = InStaticTable

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable | InStaticTable deriving (Eq,Show)

----------------------------------------------------------------

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> IO (WhichTable, Entry)
which hdrtbl idx
  | idx `isIn` hdrtbl  = (InHeaderTable,) <$> toHeaderEntry hdrtbl hidx
  | isSIndexValid sidx = return (InStaticTable, toStaticEntry sidx)
  | otherwise          = throwIO $ IndexOverrun idx
  where
    hidx = fromIndexToHIndex hdrtbl idx
    sidx = fromIndexToSIndex hdrtbl idx

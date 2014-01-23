{-# LANGUAGE TupleSections, BangPatterns #-}

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
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (foldl')
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as P
import Network.HPACK.Table.Entry
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

-- | Type for header table.
data HeaderTable = HeaderTable {
    maxNumOfEntries :: !Int
  , offset :: !Index
  , numOfEntries :: !Int
  , circularTable :: !(IOArray Index Entry)
  , headerTableSize :: !Size
  , maxHeaderTableSize :: !Size
  , reverseLookup :: !Rev
  }

adj :: Int -> Int -> Int
adj maxN x = (x + maxN) `mod` maxN

----------------------------------------------------------------

-- | Printing 'HeaderTable'.
printHeaderTable :: HeaderTable -> IO ()
printHeaderTable (HeaderTable maxN off n tbl tblsiz _ rev) = do
    es <- mapM (readArray tbl . adj maxN) [beg .. end]
    let ts = zip [1..] es
    mapM_ printEntry ts
    putStrLn $ "      Table size: " ++ show tblsiz
    print rev
  where
    beg = off + 1
    end = off + n

printEntry :: (Index,Entry) -> IO ()
printEntry (i,e) = do
    putStr "[ "
    putStr $ show i
    putStr "] (s = "
    putStr $ show $ entrySize e
    putStr ") "
    BS.putStr $ entryHeaderName e
    putStr ": "
    BS.putStrLn $ entryHeaderValue e

----------------------------------------------------------------

-- | Which table does `Index` refer to?
data WhichTable = InHeaderTable | InStaticTable deriving (Eq,Show)

data HeaderCache = None
                 | KeyOnly WhichTable Index
                 | KeyValue WhichTable Index deriving Show

----------------------------------------------------------------

newtype HIndex = HIndex Int deriving (Eq, Ord, Show)

-- | Static physical index only.
--   Index for static table.
--   Index for header table.
--   'Index' is not allowd since it is dynamic.
data PIndex = S SIndex | H HIndex deriving (Eq, Show)

instance Ord PIndex where
    (S sidx1) `compare` (S sidx2) = sidx1 `compare` sidx2
    (S _)     `compare` (H _)     = GT
    (H _)     `compare` (S _)     = LT
    (H hidx1) `compare` (H hidx2) = hidx1 `compare` hidx2

----------------------------------------------------------------

newtype Rev = Rev (HashMap HeaderName (PSQ HeaderValue PIndex)) deriving Show

----------------------------------------------------------------

fromHIndexToIndex :: HeaderTable -> HIndex -> Index
fromHIndexToIndex hdrtbl (HIndex hidx) = idx -- fixme :: Checkme
  where
    maxN = maxNumOfEntries hdrtbl
    off = offset hdrtbl
    idx = adj maxN (maxN + hidx - off)

fromIndexToHIndex :: HeaderTable -> Index -> HIndex
fromIndexToHIndex hdrtbl idx = HIndex hidx
  where
    maxN = maxNumOfEntries hdrtbl
    off = offset hdrtbl
    hidx = adj maxN (off + idx)

fromSIndexToIndex :: HeaderTable -> SIndex -> Index
fromSIndexToIndex hdrtbl sidx = fromStaticIndex sidx + numOfEntries hdrtbl

fromIndexToSIndex :: HeaderTable -> Index -> SIndex
fromIndexToSIndex hdrtbl idx = toStaticIndex sidx
  where
    sidx = idx - numOfEntries hdrtbl

----------------------------------------------------------------

-- | Creating 'HeaderTable'.
-- The default maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTable :: Size -> IO HeaderTable
newHeaderTable maxsiz = do
    tbl <- newArray (0,end) dummyEntry
    return HeaderTable {
        maxNumOfEntries = maxN
      , offset = end
      , numOfEntries = 0
      , circularTable = tbl
      , headerTableSize = 0
      , maxHeaderTableSize = maxsiz
      , reverseLookup = initialReverseLookup
      }
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

initialReverseLookup :: Rev
initialReverseLookup = ir
  where
    ss = map (S . toStaticIndex) [1..]
    shs = zip ss staticTableList
    ins !rev (!p,!h) = insert h p rev
    !ir = foldl' ins (Rev H.empty) shs

----------------------------------------------------------------

-- | Inserting 'Entry' to 'HeaderTable'.
--   New 'HeaderTable' and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> HeaderTable -> IO (HeaderTable,[Index])
insertEntry e hdrtbl = do
    (hdrtbl', is, hs) <- insertOne e hdrtbl >>= adjustTableSize
    let rev = reverseLookup hdrtbl'
        rev' = foldl' (flip delete) rev hs
        hdrtbl'' = hdrtbl' { reverseLookup = rev' }
    return (hdrtbl'', is)

insertOne :: Entry -> HeaderTable -> IO HeaderTable
insertOne e hdrtbl@(HeaderTable maxN off n tbl tsize _ rev) = do
    writeArray tbl i e
    return $ hdrtbl {
        offset = off'
      , numOfEntries = n + 1
      , headerTableSize = tsize'
      , reverseLookup = insert (entryHeader e) (H (HIndex i)) rev
      }
  where
    i = off
    tsize' = tsize + entrySize e
    off' = adj maxN (off - 1)

adjustTableSize :: HeaderTable -> IO (HeaderTable, [Index], [Header])
adjustTableSize hdrtbl = adjust hdrtbl [] []

adjust :: HeaderTable -> [Index] -> [Header] -> IO (HeaderTable, [Index], [Header])
adjust hdrtbl is hs
  | tsize <= maxtsize = return (hdrtbl, is, hs)
  | otherwise         = do
      (hdrtbl', i, h) <- removeOne hdrtbl
      adjust hdrtbl' (i:is) (h:hs)
  where
    tsize = headerTableSize hdrtbl
    maxtsize = maxHeaderTableSize hdrtbl

removeOne :: HeaderTable -> IO (HeaderTable,Index,Header)
removeOne hdrtbl@(HeaderTable maxN off n tbl tsize _ _) = do
    let i = adj maxN (off + n)
    e <- readArray tbl i
    writeArray tbl i dummyEntry -- let the entry GCed
    let tsize' = tsize - entrySize e
        h = entryHeader e
        hdrtbl' = hdrtbl {
            numOfEntries = n - 1
          , headerTableSize = tsize'
          }
    return (hdrtbl',n - 1, h)

----------------------------------------------------------------

insert :: Header -> PIndex -> Rev -> Rev
insert (k,v) p (Rev m) = case H.lookup k m of
    Nothing  -> let psq = P.singleton v p
                in Rev $ H.insert k psq m
    Just psq -> let psq' = P.insert v p psq
                in Rev $ H.adjust (const psq') k m

lookupTable :: Header -> HeaderTable -> HeaderCache
lookupTable (k,v) hdrtbl = case H.lookup k m of
    Nothing  -> None
    Just psq -> case P.lookup v psq of
        Nothing -> case P.findMin psq of
            Nothing        -> error "lookupTable"
            Just (_ :-> p) -> case p of
                S sidx -> KeyOnly InStaticTable (fromSIndexToIndex hdrtbl sidx)
                H hidx -> KeyOnly InHeaderTable (fromHIndexToIndex hdrtbl hidx)
        Just p -> case p of
            S sidx -> KeyValue InStaticTable (fromSIndexToIndex hdrtbl sidx)
            H hidx -> KeyValue InHeaderTable (fromHIndexToIndex hdrtbl hidx)
  where
    Rev m = reverseLookup hdrtbl

delete :: Header -> Rev -> Rev
delete (k,v) (Rev m) = case H.lookup k m of
    Nothing  -> error $ "delete: " ++ show k ++ " " ++ show v
    Just psq -> case P.lookup v psq of
        Nothing -> error $ "delete psq': " ++ show k ++ " " ++ show v
        Just p  -> case p of
            S _ -> Rev m
            H _ -> delete' psq
  where
    delete' psq
      | P.null psq' = Rev $ H.delete k m
      | otherwise   = Rev $ H.adjust (const psq') k m
      where
        psq' = P.delete v psq

----------------------------------------------------------------

isIn :: Int -> HeaderTable -> Bool
isIn idx hdrtbl = idx <= numOfEntries hdrtbl

-- | Which table does 'Index' belong to?
which :: HeaderTable -> Index -> IO (WhichTable, Entry)
which hdrtbl idx
  | idx `isIn` hdrtbl  = (InHeaderTable,) <$> toHeaderEntry hdrtbl hidx
  | isSIndexValid sidx = return (InStaticTable, toStaticEntry sidx)
  | otherwise          = throwIO $ IndexOverrun idx
  where
    hidx = fromIndexToHIndex hdrtbl idx
    sidx = fromIndexToSIndex hdrtbl idx

toHeaderEntry :: HeaderTable -> HIndex -> IO Entry
toHeaderEntry hdrtbl (HIndex hidx) = readArray (circularTable hdrtbl) hidx


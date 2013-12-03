module Network.HPACK.Context where

import Data.Array (listArray, (!))
import Data.Array.ST (runSTArray, writeArray)
import Data.Array.Unsafe (unsafeThaw)
import Data.List (delete, partition, (\\))
import Network.HPACK.Entry
import Network.HPACK.StaticTable
import Network.HPACK.Types

----------------------------------------------------------------

fromNaming :: Naming -> HeaderTable -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) hdrtbl = case hdrtbl .!. idx of
    InHeaderTable e -> Right $ entryHeaderName e
    InStaticTable e -> Right $ entryHeaderName e
    IndexError      -> Left IndexOverrun

----------------------------------------------------------------

newEntry :: Entry -> Context -> Context
newEntry e (Context hdrtbl oldref newref hdrset) = ctx
  where
    (hdrtbl', is) = insertEntry e hdrtbl
    oldref' = adjustIndex $ removeIndices is oldref
    newref' = addIndex 1 $ adjustIndex $ removeIndices is newref
    hdrset' = fromEntry e : hdrset
    ctx = Context hdrtbl' oldref' newref' hdrset'

pushRef :: Index -> Entry -> Context -> Context
pushRef idx e (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = fromEntry e : hdrset
    newref' = addIndex idx newref
    ctx = Context hdrtbl oldref newref' hdrset'

emitOnly :: Header -> Context -> Context
emitOnly h (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = h : hdrset
    ctx = Context hdrtbl oldref newref hdrset'

----------------------------------------------------------------

insertEntry :: Entry -> HeaderTable -> (HeaderTable,[Index])
insertEntry e hdrtbl = adjustTableSize $ insertOne e hdrtbl

insertOne :: Entry -> HeaderTable -> HeaderTable
insertOne e hdrtbl@(HeaderTable maxN off n tbl tsize _) = hdrtbl'
  where
    i = off
    tbl' = modifyTable tbl i e
    tsize' = tsize + entrySize e
    off' = (off - 1 + maxN) `mod` maxN
    hdrtbl' = hdrtbl {
        offset = off'
      , numOfEntries = n + 1
      , circularTable = tbl'
      , headerTableSize = tsize'
      }

adjustTableSize :: HeaderTable -> (HeaderTable, [Index])
adjustTableSize hdrtbl = adjust hdrtbl []

adjust :: HeaderTable -> [Index] -> (HeaderTable, [Index])
adjust hdrtbl is
  | tsize <= maxtsize = (hdrtbl, is)
  | otherwise         = let (hdrtbl', i) = removeOne hdrtbl
                        in adjust hdrtbl' (i:is)
  where
    tsize = headerTableSize hdrtbl
    maxtsize = maxHeaderTableSize hdrtbl

removeOne :: HeaderTable -> (HeaderTable,Index)
removeOne hdrtbl@(HeaderTable maxN off n tbl tsize _) = (hdrtbl',i)
  where
    i = (off + n + maxN) `mod` maxN
    e = tbl ! i
    tbl' = modifyTable tbl i dummyEntry -- let the entry GCed
    tsize' = tsize - entrySize e
    hdrtbl' = hdrtbl {
        numOfEntries = n - 1
      , circularTable = tbl'
      , headerTableSize = tsize'
      }

modifyTable :: Table -> Index -> Entry -> Table
modifyTable tbl i e = runSTArray $ do
    arr <- unsafeThaw tbl
    writeArray arr i e
    return arr

(.!.) :: HeaderTable -> Index -> WhichTable
HeaderTable maxN off n tbl _ _ .!. idx
  | idx <= n                        = InHeaderTable $ tbl ! pidx
  | 1 <= stcidx && stcidx <= stcsiz = InStaticTable $ stctbl ! stcidx
  | otherwise                       = IndexError
  where
    StaticTable stcsiz stctbl = staticTable
    stcidx = idx - n
    pidx = (off + idx + maxN) `mod` maxN

-- maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTable :: Size -> HeaderTable
newHeaderTable maxsiz = HeaderTable {
    maxNumOfEntries = maxN
  , offset = end
  , numOfEntries = 0
  , circularTable = listArray (0, end) $ replicate maxN dummyEntry
  , headerTableSize = 0
  , maxHeaderTableSize = maxsiz
  }
  where
    maxN = maxsiz `div` headerSizeMagicNumber
    end = maxN - 1

----------------------------------------------------------------

isPresent :: Index -> ReferenceSet -> Bool
isPresent idx (ReferenceSet is) = idx `elem` is

addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet is) = ReferenceSet $ idx : is

removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet is) = ReferenceSet $ delete idx is

removeIndices :: [Index] -> ReferenceSet -> ReferenceSet
removeIndices idcs (ReferenceSet is) = ReferenceSet $ is \\ idcs

adjustIndex :: ReferenceSet -> ReferenceSet
adjustIndex (ReferenceSet is) = ReferenceSet $ map (+1) is

allEntries :: ReferenceSet -> HeaderTable -> Either DecodeError HeaderSet
allEntries (ReferenceSet is) hdrtbl
  | null ls   = Right xs
  | otherwise = Left IndexOverrun
  where
    ws = map (\i -> hdrtbl .!. i) is
    (ls,rs) = partition (== IndexError) ws
    fromWhich (InHeaderTable e) = e
    fromWhich (InStaticTable e) = e
    fromWhich _                 = error "fromWhich"
    xs = map (fromEntry . fromWhich) rs

mergeReferenceSet :: ReferenceSet -> ReferenceSet -> ReferenceSet
mergeReferenceSet (ReferenceSet xs) (ReferenceSet ys) = ReferenceSet $ xs ++ ys

----------------------------------------------------------------

emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet []

emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

----------------------------------------------------------------

newContext :: Size -> Context
newContext maxsiz = Context (newHeaderTable maxsiz)
                            emptyReferenceSet
                            emptyReferenceSet
                            emptyHeaderSet

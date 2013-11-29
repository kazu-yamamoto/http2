module Network.HPACK.Context where

import Data.Array (listArray, (!))
import Data.Array.ST (runSTArray, writeArray)
import Data.Array.Unsafe (unsafeThaw)
import Data.List (delete, partition)
import Network.HPACK.Entry
import Network.HPACK.StaticTable
import Network.HPACK.Types

----------------------------------------------------------------

fromNaming :: Naming -> HeaderTable -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) hdrtbl = case magicalIndex idx hdrtbl of
    InHeaderTable e -> Right $ entryHeaderName e
    InStaticTable e -> Right $ entryHeaderName e
    IndexError      -> Left IndexOverrun

----------------------------------------------------------------

newEntry :: Entry -> Context -> Context
newEntry e (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrtbl' = insertEntry e hdrtbl
    oldref' = adjustIndex oldref
    newref' = addIndex 1 $ adjustIndex newref
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

-- FIXME
insertEntry :: Entry -> HeaderTable -> HeaderTable
insertEntry e hdrtbl = hdrtbl'
  where
    tsize = headerTableSize hdrtbl + entrySize e
    off' = offset hdrtbl
    off = off' - 1
    tbl = runSTArray $ do
        arr <- unsafeThaw $ circularTable hdrtbl
        writeArray arr off' e
        return arr
    len = numOfEntries hdrtbl + 1
    hdrtbl' = hdrtbl {
        offset = off
      , numOfEntries = len
      , circularTable = tbl
      , headerTableSize = tsize
      }

magicalIndex :: Index -> HeaderTable -> WhichTable
magicalIndex idx (HeaderTable siz off len tbl _ _)
  | idx <= len                      = InHeaderTable $ tbl ! ajtidx
  | 1 <= stcidx && stcidx <= stcsiz = InStaticTable $ stctbl ! stcidx
  | otherwise                       = IndexError
  where
    StaticTable stcsiz stctbl = staticTable
    stcidx = idx - len
    pidx = off + idx
    ajtidx
      | pidx <= siz = pidx
      | otherwise   = idx - pidx

-- maxHeaderTableSize is 4096 bytes,
-- an array has 128 entries, resulting 1024 bytes in 64bit machine
newHeaderTable :: Size -> HeaderTable
newHeaderTable maxsiz = HeaderTable {
    maxNumOfEntries = maxNum
  , offset = end
  , numOfEntries = 0
  , circularTable = listArray (0, end) $ replicate maxNum dummyEntry
  , headerTableSize = 0
  , maxHeaderTableSize = maxsiz
  }
  where
    maxNum = maxsiz `div` headerSizeMagicNumber
    end = maxNum - 1

----------------------------------------------------------------

isPresent :: Index -> ReferenceSet -> Bool
isPresent idx (ReferenceSet is) = idx `elem` is

addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet is) = ReferenceSet $ idx : is

removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet is) = ReferenceSet $ delete idx is

adjustIndex :: ReferenceSet -> ReferenceSet
adjustIndex (ReferenceSet is) = ReferenceSet $ map (+1) is

allEntries :: ReferenceSet -> HeaderTable -> Either DecodeError HeaderSet
allEntries (ReferenceSet is) hdrtbl
  | null ls   = Right xs
  | otherwise = Left IndexOverrun
  where
    ws = map (\i -> magicalIndex i hdrtbl) is
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

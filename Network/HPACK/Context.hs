module Network.HPACK.Context where

import Data.Array (array, (!))
import Data.List (delete)
import Network.HPACK.Types
import Network.HPACK.StaticTable

----------------------------------------------------------------

fromNaming :: Naming -> HeaderTable -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) hdrtbl = case magicalIndex idx hdrtbl of
    InHeaderTable (k,_) -> Right k
    InStaticTable (k,_) -> Right k
    IndexError          -> Left IndexOverrun

----------------------------------------------------------------

insertEntry :: Header -> HeaderTable -> ReferenceSet
            -> (HeaderTable, ReferenceSet)
insertEntry h hdrtbl refset = (hdrtbl', refset')
  where
    hdrtbl' = undefined
    refset' = adjustIndex refset

-- FIXME: not implemented yet
magicalIndex :: Index -> HeaderTable -> WhichTable
magicalIndex idx (HeaderTable siz off len tbl)
  | len < idx && idx - len <= stcsiz = InStaticTable $ stctbl ! (idx - len)
  | otherwise = IndexError
  where
    StaticTable stcsiz stctbl = staticTable

----------------------------------------------------------------

isPresent :: Index -> ReferenceSet -> Bool
isPresent idx (ReferenceSet is) = idx `elem` is

addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet is) = ReferenceSet $ idx : is

removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet is) = ReferenceSet $ delete idx is

adjustIndex :: ReferenceSet -> ReferenceSet
adjustIndex (ReferenceSet is) = ReferenceSet $ map (+1) is

----------------------------------------------------------------

emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet []

emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

----------------------------------------------------------------

newContext :: Size -> Context
newContext siz = Context hdrtbl emptyReferenceSet emptyHeaderSet
  where
    hdrtbl = HeaderTable siz 1 0 $ array (1,siz) [] -- FIXME

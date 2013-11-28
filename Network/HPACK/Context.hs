module Network.HPACK.Context where

import Data.List (delete)
import Network.HPACK.Types

----------------------------------------------------------------

fromNaming :: Naming -> HeaderTable -> Either DecodeError HeaderName
fromNaming (Lit k)   _  = Right k
fromNaming (Idx idx) hdrtbl = case magicalIndex idx hdrtbl of
    InHeaderTable k _ -> Right k
    InStaticTable k _ -> Right k
    IndexError        -> Left IndexOverrun

----------------------------------------------------------------

insertEntry :: HeaderName -> HeaderValue -> HeaderTable -> ReferenceSet
            -> (HeaderTable, ReferenceSet)
insertEntry = undefined

----------------------------------------------------------------

isPresent :: Index -> ReferenceSet -> Bool
isPresent idx (ReferenceSet is) = idx `elem` is

addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet is) = ReferenceSet $ idx : is

removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet is) = ReferenceSet $ delete idx is

----------------------------------------------------------------

emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet []

----------------------------------------------------------------

newContext :: Size -> Context
newContext = undefined

magicalIndex :: Index -> HeaderTable -> WhichTable
magicalIndex = undefined

module Network.HPACK.ReferenceSet where

import Data.List (delete, (\\))
import Network.HPACK.Types

----------------------------------------------------------------

data ReferenceSet = ReferenceSet [Index] deriving Show

emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet []

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

mergeReferenceSet :: ReferenceSet -> ReferenceSet -> ReferenceSet
mergeReferenceSet (ReferenceSet xs) (ReferenceSet ys) = ReferenceSet $ xs ++ ys

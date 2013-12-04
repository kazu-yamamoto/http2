module Network.HPACK.ReferenceSet (
  -- * Type
    ReferenceSet
  , emptyReferenceSet
  -- * Index and reference set
  , getIndices
  , isMember
  , addIndex
  , removeIndex
  , removeIndices
  -- * Managing reference set
  , adjustReferenceSet
  , mergeReferenceSet
  ) where

import Data.List (delete, (\\))
import Network.HPACK.Types

----------------------------------------------------------------

-- | Type for the reference set.
data ReferenceSet = ReferenceSet [Index] deriving Show

-- | Empty reference set.
emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet []

----------------------------------------------------------------

-- | Get all 'Index' from 'ReferenceSet'.
getIndices :: ReferenceSet -> [Index]
getIndices (ReferenceSet is) = is

-- | Is 'Index' a member of 'ReferenceSet'?
isMember :: Index -> ReferenceSet -> Bool
isMember idx (ReferenceSet is) = idx `elem` is

-- | Adding 'Index' to 'ReferenceSet'.
addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet is) = ReferenceSet $ idx : is

-- | Removing 'Index' from 'ReferenceSet'.
removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet is) = ReferenceSet $ delete idx is

-- | Removing a set of 'Index' from 'ReferenceSet'.
removeIndices :: [Index] -> ReferenceSet -> ReferenceSet
removeIndices idcs (ReferenceSet is) = ReferenceSet $ is \\ idcs

----------------------------------------------------------------

-- | Incrementing all 'Index' by one.
adjustReferenceSet :: ReferenceSet -> ReferenceSet
adjustReferenceSet (ReferenceSet is) = ReferenceSet $ map (+1) is

-- | Merging two 'ReferenceSet'.
mergeReferenceSet :: ReferenceSet -> ReferenceSet -> ReferenceSet
mergeReferenceSet (ReferenceSet xs) (ReferenceSet ys) = ReferenceSet $ xs ++ ys

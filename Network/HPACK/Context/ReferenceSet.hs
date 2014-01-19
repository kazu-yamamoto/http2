module Network.HPACK.Context.ReferenceSet (
  -- * Type
    ReferenceSet
  , emptyReferenceSet
  -- * Index and reference set
  , isMember
  , addIndex
  , removeIndex
  , removeIndices
  -- * Managing reference set
  , getNotEmittedIndices
  , adjustReferenceSet
  , renew
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.HPACK.Table

----------------------------------------------------------------

data Status = NotEmitted | Emitted deriving (Eq,Show)

-- | Type for the reference set.
newtype ReferenceSet = ReferenceSet (Map Index Status) deriving Show
-- Must not use IntMap because its mapKeysMonotonic is slow

-- | Empty reference set.
emptyReferenceSet :: ReferenceSet
emptyReferenceSet = ReferenceSet M.empty

----------------------------------------------------------------

-- | Is 'Index' a member of 'ReferenceSet'?
isMember :: Index -> ReferenceSet -> Bool
isMember idx (ReferenceSet m) = idx `M.member` m

-- | Adding 'Index' to 'ReferenceSet'.
addIndex :: Index -> ReferenceSet -> ReferenceSet
addIndex idx (ReferenceSet m) = ReferenceSet $ M.insert idx Emitted m

-- | Removing 'Index' from 'ReferenceSet'.
removeIndex :: Index -> ReferenceSet -> ReferenceSet
removeIndex idx (ReferenceSet m) = ReferenceSet $ M.delete idx m

-- | Removing a set of 'Index' from 'ReferenceSet'.
removeIndices :: [Index] -> ReferenceSet -> ReferenceSet
removeIndices idcs (ReferenceSet m) = ReferenceSet $ foldl' (flip M.delete) m idcs

----------------------------------------------------------------

-- | Get all not emitted 'Index' from 'ReferenceSet'.
getNotEmittedIndices :: ReferenceSet -> [Index]
getNotEmittedIndices (ReferenceSet m) = M.keys $ M.filter (== NotEmitted) m

-- | Renewing 'ReferenceSet' for the next step.
renew :: ReferenceSet -> ReferenceSet
renew (ReferenceSet m) = ReferenceSet $ M.map (const NotEmitted) m

-- | Incrementing all 'Index' by one.
adjustReferenceSet :: ReferenceSet -> ReferenceSet
adjustReferenceSet (ReferenceSet m) = ReferenceSet $ M.mapKeysMonotonic (+1) m

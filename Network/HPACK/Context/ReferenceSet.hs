module Network.HPACK.Context.ReferenceSet (
  -- * Type
    ReferenceSet
  , emptyReferenceSet
  -- * Index and reference set
  , isMember
  , addIndex
  , removeIndex
  , removeIndices
  , restrictIndices
  -- * Managing reference set
  , getNotEmittedIndices
  , adjustReferenceSet
  , renewForEncoding
  , renewForDecoding
  , Sequence(..)
  , lookupAndUpdate
  , getCommon
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.HPACK.Table

----------------------------------------------------------------

data Status = Old        -- E:   Initial state for encoding
            | NotEmitted -- E&D: Initial state for decoding
            | Emitted    -- E&D:
            deriving (Eq,Show)

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

restrictIndices :: Index -> ReferenceSet -> ReferenceSet
restrictIndices idx (ReferenceSet m) =
    ReferenceSet $ M.filterWithKey (\k _ -> k <= idx) m

----------------------------------------------------------------

-- | Get all not emitted 'Index' from 'ReferenceSet'.
getNotEmittedIndices :: ReferenceSet -> [Index]
getNotEmittedIndices (ReferenceSet m) = M.keys $ M.filter (== NotEmitted) m

-- | Renewing 'ReferenceSet' for the next encoding step.
renewForEncoding :: ReferenceSet -> ([Index],ReferenceSet)
renewForEncoding (ReferenceSet m) = (removedIndces, ReferenceSet m')
  where
    (oldm,newm) = M.partition (== Old) m
    removedIndces = M.keys oldm
    m' = M.map (const Old) newm

-- | Renewing 'ReferenceSet' for the next decoding step.
renewForDecoding :: ReferenceSet -> ReferenceSet
renewForDecoding (ReferenceSet m) = ReferenceSet m'
  where
    m' = M.map (const NotEmitted) m

-- | Incrementing all 'Index' by one.
adjustReferenceSet :: ReferenceSet -> ReferenceSet
adjustReferenceSet (ReferenceSet m) = ReferenceSet $ M.mapKeysMonotonic (+1) m

data Sequence = Z | E0 | E2 | E4

lookupAndUpdate :: Index -> ReferenceSet -> (Sequence,ReferenceSet)
lookupAndUpdate idx rs@(ReferenceSet m) = case M.lookup idx m of
    Nothing         -> (Z,  rs)
    Just Old        -> (E0, ReferenceSet $ M.adjust (const NotEmitted) idx m)
    Just NotEmitted -> (E4, ReferenceSet $ M.adjust (const Emitted) idx m)
    Just Emitted    -> (E2, rs)

getCommon :: [Index] -> ReferenceSet -> [Index]
getCommon is (ReferenceSet m) = go is []
  where
    go []     ret = ret
    go (n:ns) ret = case M.lookup n m of
        Just NotEmitted -> go ns (n:ret)
        _               -> go ns ret

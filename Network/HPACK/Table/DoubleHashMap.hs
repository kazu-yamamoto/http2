{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.DoubleHashMap (
    DoubleHashMap
  , empty
  , insert
  , delete
  , fromList
  , deleteList
  , Res(..)
  , search
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (foldl')
import Network.HPACK.Types

newtype DoubleHashMap a =
    DoubleHashMap (HashMap HeaderName (HashMap HeaderValue a)) deriving Show

empty :: DoubleHashMap a
empty = DoubleHashMap H.empty

insert :: Ord a => Header -> a -> DoubleHashMap a -> DoubleHashMap a
insert (k,v) p (DoubleHashMap m) = case H.lookup k m of
    Nothing    -> let inner = H.singleton v p
                  in DoubleHashMap $ H.insert k inner m
    Just inner -> let inner' = H.insert v p inner
                  in DoubleHashMap $ H.adjust (const inner') k m

delete :: Ord a => Header -> DoubleHashMap a -> DoubleHashMap a
delete (k,v) dhm@(DoubleHashMap outer) = case H.lookup k outer of
    Nothing  -> dhm -- Non-smart implementation makes duplicate keys.
                    -- It is likely to happen to delete the same key
                    -- in multiple times.
    Just inner -> case H.lookup v inner of
        Nothing -> dhm -- see above
        _       -> delete' inner
  where
    delete' inner
      | H.null inner' = DoubleHashMap $ H.delete k outer
      | otherwise     = DoubleHashMap $ H.adjust (const inner') k outer
      where
        inner' = H.delete v inner

fromList :: Ord a => [(a,Header)] -> DoubleHashMap a
fromList alist = hashinner
  where
    ins !hp (!a,!dhm) = insert dhm a hp
    !hashinner = foldl' ins empty alist

deleteList :: Ord a => [Header] -> DoubleHashMap a -> DoubleHashMap a
deleteList hs hp = foldl' (flip delete) hp hs

data Res a = N | K a | KV a

search :: Ord a => Header -> DoubleHashMap a -> Res a
search (k,v) (DoubleHashMap outer) = case H.lookup k outer of
    Nothing  -> N
    Just inner -> case H.lookup v inner of
        Nothing -> case top inner of
            Nothing -> error "DoubleHashMap.search"
            Just a  -> K a
        Just a -> KV a

-- | Take an arbitrary entry. O(1) thanks to lazy evaluation.
top :: HashMap k v -> Maybe v
top = H.foldr (\v _ -> Just v) Nothing

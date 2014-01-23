{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.HashPSQ (
    HashPSQ
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
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as P
import Network.HPACK.Types

newtype HashPSQ p = HashPSQ (HashMap HeaderName (PSQ HeaderValue p)) deriving Show

empty :: HashPSQ p
empty = HashPSQ H.empty

insert :: Ord p => Header -> p -> HashPSQ p -> HashPSQ p
insert (k,v) p (HashPSQ m) = case H.lookup k m of
    Nothing  -> let psq = P.singleton v p
                in HashPSQ $ H.insert k psq m
    Just psq -> let psq' = P.insert v p psq
                in HashPSQ $ H.adjust (const psq') k m

delete :: Ord p => Header -> HashPSQ p -> HashPSQ p
delete (k,v) (HashPSQ m) = case H.lookup k m of
    Nothing  -> error $ "HashPSQ.delete: " ++ show k ++ " " ++ show v
    Just psq -> case P.lookup v psq of
        Nothing -> error $ "HashPSQ.delete psq': " ++ show k ++ " " ++ show v
        _       -> delete' psq
  where
    delete' psq
      | P.null psq' = HashPSQ $ H.delete k m
      | otherwise   = HashPSQ $ H.adjust (const psq') k m
      where
        psq' = P.delete v psq

fromList :: Ord p => [(p,Header)] -> HashPSQ p
fromList alst = hashpsq
  where
    ins !hp (!p,!h) = insert h p hp
    !hashpsq = foldl' ins empty alst

deleteList :: Ord p => [Header] -> HashPSQ p -> HashPSQ p
deleteList hs hp = foldl' (flip delete) hp hs

data Res p = N | K p | KV p

search :: Ord p => Header -> HashPSQ p -> Res p
search (k,v) (HashPSQ m) = case H.lookup k m of
    Nothing  -> N
    Just psq -> case P.lookup v psq of
        Nothing -> case P.findMin psq of
            Nothing        -> error "HashPSQ.lookup"
            Just (_ :-> p) -> K p
        Just p -> KV p

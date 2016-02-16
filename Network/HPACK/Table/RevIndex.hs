{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.RevIndex where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Network.HPACK.Types
import Network.HPACK.Table.Static

-- Physical array index for Dynamic Table.
-- Defining here due to dependency, sigh.
newtype DIndex = DIndex Int deriving (Eq, Ord, Show)

data Inner = Inner (HashMap HeaderValue SIndex)
                   (HashMap HeaderValue DIndex) deriving Show

newtype Outer = Outer (HashMap HeaderName Inner) deriving Show

defaultRevIndex :: Outer
defaultRevIndex = Outer $! foldr op H.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = H.alter f k m
      where
        f Nothing              = let !ss = H.singleton v i
                                 in Just $! Inner ss H.empty
        f (Just (Inner ss ds)) = let !ss' = H.insert v i ss
                                 in Just $! Inner ss' ds

insertDynamic :: Header -> DIndex -> Outer -> Outer
insertDynamic (k,v) didx (Outer rev) = Outer $! H.alter f k rev
  where
    f Nothing              = let !ds = H.singleton v didx
                             in Just $! Inner H.empty ds
    f (Just (Inner ss ds)) = let !ds' = H.insert v didx ds
                             in Just $! Inner ss ds'

deleteDynamic :: Header -> HashMap HeaderName Inner -> HashMap HeaderName Inner
deleteDynamic (k,v) rev = H.alter f k rev
  where
    f Nothing              = Nothing
    f (Just (Inner ss ds))
      | null ds'  = if H.null ss then Nothing else Just $! Inner ss H.empty
      | otherwise = Just $! Inner ss ds'
      where
        !ds' = H.delete v ds

deleteDynamicList :: [Header] -> Outer -> Outer
deleteDynamicList hs (Outer rev) = Outer $! foldr deleteDynamic rev hs

--- | Take an arbitrary entry. O(1) thanks to lazy evaluation.
top :: HashMap k v -> Maybe v
top = H.foldr (\v _ -> Just v) Nothing
{-# INLINE top #-}

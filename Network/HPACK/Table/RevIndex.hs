{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.RevIndex (
    Outer
  , Inner
  , defaultRevIndex
  , insertRevIndex
  , deleteRevIndexList
  , lookupOuter
  , lookupInner
  , topInner
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Network.HPACK.Types
import Network.HPACK.Table.Static

data Inner = Inner (HashMap HeaderValue HIndex) deriving Show

newtype Outer = Outer (HashMap HeaderName Inner) deriving Show

defaultRevIndex :: Outer
defaultRevIndex = Outer $! foldr op H.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = H.alter f k m
      where
        f Nothing           = let !hh = H.singleton v i
                              in Just $! Inner hh
        f (Just (Inner hh)) = let !hh' = H.insert v i hh
                              in Just $! Inner hh'

insertRevIndex :: Header -> HIndex -> Outer -> Outer
insertRevIndex (k,v) hidx (Outer rev) = Outer $! H.alter f k rev
  where
    f Nothing           = let !hh = H.singleton v hidx
                          in Just $! Inner hh
    f (Just (Inner hh)) = let !hh' = H.insert v hidx hh
                          in Just $! Inner hh'

deleteRevIndex :: Header -> HashMap HeaderName Inner -> HashMap HeaderName Inner
deleteRevIndex (k,v) rev = H.alter f k rev
  where
    f Nothing              = Nothing
    f (Just (Inner hh))
      | null hh'           = Nothing
      | otherwise          = Just $! Inner hh'
      where
        !hh' = H.delete v hh

deleteRevIndexList :: [Header] -> Outer -> Outer
deleteRevIndexList hs (Outer rev) = Outer $! foldr deleteRevIndex rev hs

lookupOuter :: HeaderName -> Outer -> Maybe Inner
lookupOuter k (Outer rev) = H.lookup k rev
{-# INLINE lookupOuter #-}

lookupInner :: HeaderValue -> Inner -> Maybe HIndex
lookupInner v (Inner hh) = H.lookup v hh
{-# INLINE lookupInner #-}

--- | Take an arbitrary entry. O(1) thanks to lazy evaluation.
topInner :: Inner -> Maybe HIndex
topInner (Inner hh) = H.foldr (\v _ -> Just v) Nothing hh
{-# INLINE topInner #-}

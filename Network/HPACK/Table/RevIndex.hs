{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.RevIndex (
    Outer
  , Inner
  , defaultRevIndex
  , insertRevIndex
  , deleteRevIndexList
  , lookupOuter
  , lookupInner
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as H
import Network.HPACK.Types
import Network.HPACK.Table.Static

data Inner = Inner (Map HeaderValue HIndex) deriving Show

newtype Outer = Outer (Map HeaderName Inner) deriving Show

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

deleteRevIndex :: Header -> Map HeaderName Inner -> Map HeaderName Inner
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

lookupInner :: HeaderValue -> Inner -> Either HIndex HIndex
lookupInner v (Inner hh) = case H.lookup v hh of
    Just hidx -> Right hidx
    Nothing   -> case H.foldr (\x _ -> Just x) Nothing hh of
        Just hidx -> Left hidx
        Nothing   -> error "lookupInner"
{-# INLINE lookupInner #-}

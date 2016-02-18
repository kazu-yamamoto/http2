{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Table.RevIndex (
    RevIndex
  , RevResult(..)
  , defaultRevIndex
  , insertRevIndex
  , deleteRevIndexList
  , lookupRevIndex
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as H
import Network.HPACK.Types
import Network.HPACK.Table.Static

newtype RevIndex = RevIndex (Map HeaderName Inner) deriving Show

data Inner = Inner (Map HeaderValue HIndex) deriving Show

data RevResult = N | K HIndex | KV HIndex

defaultRevIndex :: RevIndex
defaultRevIndex = RevIndex $! foldr op H.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = H.alter f k m
      where
        f Nothing           = let !hh = H.singleton v i
                              in Just $! Inner hh
        f (Just (Inner hh)) = let !hh' = H.insert v i hh
                              in Just $! Inner hh'

insertRevIndex :: Header -> HIndex -> RevIndex -> RevIndex
insertRevIndex (k,v) hidx (RevIndex rev) = RevIndex $! H.alter f k rev
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
      | H.null hh'         = Nothing
      | otherwise          = Just $! Inner hh'
      where
        !hh' = H.delete v hh

deleteRevIndexList :: [Header] -> RevIndex -> RevIndex
deleteRevIndexList hs (RevIndex rev) = RevIndex $! foldr deleteRevIndex rev hs

{-# INLINE lookupRevIndex #-}
lookupRevIndex :: HeaderName -> HeaderValue -> RevIndex -> RevResult
lookupRevIndex k v (RevIndex rev) = case H.lookup k rev of
    Nothing         -> N
    Just (Inner hh) -> case H.lookup v hh of
        Just hidx -> KV hidx
        Nothing   -> case H.foldr (\x _ -> Just x) Nothing hh of
            Just hidx -> K hidx
            Nothing   -> error "lookupRevIndex"

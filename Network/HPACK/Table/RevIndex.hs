module Network.HPACK.Table.RevIndex where

import Data.Map (Map)
import qualified Data.Map as M
import Network.HPACK.Types
import Network.HPACK.Table.Static

-- Physical array index for Dynamic Table.
-- Defining here due to dependency, sigh.
newtype DIndex = DIndex Int deriving (Eq, Ord, Show)

data Inner = Inner [(HeaderValue, SIndex)] [(HeaderValue, DIndex)] deriving Show

newtype Outer = Outer (Map HeaderName Inner) deriving Show

defaultRevIndex :: Outer
defaultRevIndex = Outer $ foldr op M.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = M.alter f k m
      where
        f Nothing              = Just $ Inner [(v,i)]    []
        f (Just (Inner ss ds)) = Just $ Inner ((v,i):ss) ds

insertDynamic :: Header -> DIndex -> Outer -> Outer
insertDynamic (k,v) didx (Outer rev) = Outer $ M.alter f k rev
  where
    f Nothing              = Just $ Inner [] [(v,didx)]
    f (Just (Inner ss ds)) = Just $ Inner ss ((v,didx):ds)

deleteDynamic :: Header -> Map HeaderName Inner -> Map HeaderName Inner
deleteDynamic (k,v) rev = M.alter f k rev
  where
    f Nothing              = Nothing
    f (Just (Inner ss ds)) = case filter (\(a,_) -> a /= v) ds of
        []  -> Nothing
        ds' -> Just $ Inner ss ds'

deleteDynamicList :: [Header] -> Outer -> Outer
deleteDynamicList hs (Outer rev) = Outer $ foldr deleteDynamic rev hs

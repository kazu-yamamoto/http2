module Network.HPACK2.Table.RevIndex where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.HPACK2.Types
import Network.HPACK2.Table.Static

-- Physical array index for Dynamic Table.
-- Defining here due to dependency, sigh.
newtype DIndex = DIndex Int deriving (Eq, Ord, Show)

data Inner = Inner [(HeaderValue, SIndex)] [(HeaderValue, DIndex)] deriving Show

newtype Outer = Outer (Map HeaderName Inner) deriving Show

defaultRevIndex :: Outer
defaultRevIndex = Outer $! foldr op M.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = M.alter f k m
      where
        f Nothing              = Just $! Inner [(v,i)]    []
        f (Just (Inner ss ds)) = let ss' = (v,i):ss
                                 in Just $! Inner ss' ds

insertDynamic :: Header -> DIndex -> Outer -> Outer
insertDynamic (k,v) didx (Outer rev) = Outer $! M.alter f k rev
  where
    f Nothing              = Just $! Inner [] [(v,didx)]
    f (Just (Inner ss ds)) = let ds' = (v,didx):ds
                             in Just $! Inner ss ds'

deleteDynamic :: Header -> Map HeaderName Inner -> Map HeaderName Inner
deleteDynamic (k,v) rev = M.alter f k rev
  where
    f Nothing              = Nothing
    f (Just (Inner ss ds)) = case filter (\(a,_) -> a /= v) ds of
        []  -> case ss of
            [] -> Nothing
            _  -> Just $! Inner ss []
        ds' -> Just $! Inner ss ds'

deleteDynamicList :: [Header] -> Outer -> Outer
deleteDynamicList hs (Outer rev) = Outer $! foldr deleteDynamic rev hs

module Network.HPACK.Table.RevIndex where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Network.HPACK.Types
import Network.HPACK.Table.Static

-- Physical array index for Dynamic Table.
-- Defining here due to dependency, sigh.
newtype DIndex = DIndex Int deriving (Eq, Ord, Show)

data Inner = Inner [(HeaderValue, SIndex)] [(HeaderValue, DIndex)] deriving Show

newtype Outer = Outer (HashMap HeaderName Inner) deriving Show

defaultRevIndex :: Outer
defaultRevIndex = Outer $! foldr op H.empty lst
  where
    lst = zip staticTableList $ map SIndex [1..]
    op ((k,v),i) m = H.alter f k m
      where
        f Nothing              = Just $! Inner [(v,i)]    []
        f (Just (Inner ss ds)) = let ss' = (v,i):ss
                                 in Just $! Inner ss' ds

insertDynamic :: Header -> DIndex -> Outer -> Outer
insertDynamic (k,v) didx (Outer rev) = Outer $! H.alter f k rev
  where
    f Nothing              = Just $! Inner [] [(v,didx)]
    f (Just (Inner ss ds)) = let ds' = (v,didx):ds
                             in Just $! Inner ss ds'

deleteDynamic :: Header -> HashMap HeaderName Inner -> HashMap HeaderName Inner
deleteDynamic (k,v) rev = H.alter f k rev
  where
    f Nothing              = Nothing
    f (Just (Inner ss ds)) = case filter (\(a,_) -> a /= v) ds of
        []  -> case ss of
            [] -> Nothing
            _  -> Just $! Inner ss []
        ds' -> Just $! Inner ss ds'

deleteDynamicList :: [Header] -> Outer -> Outer
deleteDynamicList hs (Outer rev) = Outer $! foldr deleteDynamic rev hs

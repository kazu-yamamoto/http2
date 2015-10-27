{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Heap (
    PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , delete
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.Array (Array, listArray, (!))
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue {
    baseDeficit :: {-# UNPACK #-} !Deficit
  , deficitMap :: IntMap Deficit
  , queue :: Heap (Entry a)
  }

----------------------------------------------------------------

data Entry a = Entry {-# UNPACK #-} !Deficit
                     {-# UNPACK #-} !Key
                     {-# UNPACK #-} !Weight
                     a

instance Eq (Entry a) where
    Entry d1 _ _ _ == Entry d2 _ _ _ = d1 == d2

instance Ord (Entry a) where
    Entry d1 _ _ _ <  Entry d2 _ _ _ = d1 <  d2
    Entry d1 _ _ _ <= Entry d2 _ _ _ = d1 <= d2

----------------------------------------------------------------

deficitSteps :: Int
deficitSteps = 65536

deficitList :: [Deficit]
deficitList = map calc idxs
  where
    idxs = [1..256] :: [Double]
    calc w = round (fromIntegral deficitSteps / w)

deficitTable :: Array Int Deficit
deficitTable = listArray (1,256) deficitList

weightToDeficit :: Weight -> Deficit
weightToDeficit w = deficitTable ! w

----------------------------------------------------------------

empty :: PriorityQueue a
empty = PriorityQueue 0 I.empty H.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty PriorityQueue{..} = H.null queue

enqueue :: Key -> Weight -> a -> PriorityQueue a -> PriorityQueue a
enqueue k w x PriorityQueue{..} = PriorityQueue baseDeficit deficitMap' queue'
  where
    !b = case I.lookup k deficitMap of
        Nothing      -> baseDeficit
        Just deficit -> deficit
    !deficit' = b + weightToDeficit w
    !deficitMap' = I.insert k deficit' deficitMap
    !ent = Entry deficit' k w x
    !queue' = H.insert ent queue

dequeue :: PriorityQueue a -> Maybe (Key, Weight, a, PriorityQueue a)
dequeue PriorityQueue{..} = case H.uncons queue of
    Nothing                     -> Nothing
    Just (Entry d k w x, queue')
      | H.null queue' -> Just (k, w, x, empty) -- reset the deficit base
      | otherwise     -> Just (k, w, x, PriorityQueue d deficitMap queue')

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k PriorityQueue{..}
  | H.null queue' = (mx, PriorityQueue baseDeficit deficitMap queue')
  | otherwise     = let Entry d _ _ _ = H.minimum queue'
                    in (mx, PriorityQueue d deficitMap queue')
  where
    (!h, !queue') = H.partition (\(Entry _ k' _ _) -> k == k') queue
    !mx
      | H.null h  = Nothing
      | otherwise = let Entry _ _ _ x = H.minimum h
                    in Just x

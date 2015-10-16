{-# LANGUAGE BangPatterns #-}

module Network.HTTP2.Priority.Heap (
    Entry
  , newEntry
  , renewEntry
  , item
  , PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  ) where

import Data.Array (Array, listArray, (!))
import Data.Heap (Heap)
import qualified Data.Heap as H

----------------------------------------------------------------

type Weight = Int
type Pri = Int

data Entry a = Entry a
                 {-# UNPACK #-} !Weight
                 {-# UNPACK #-} !Pri deriving Show

instance Eq (Entry a) where
    Entry _ _ p1 == Entry _ _ p2 = p1 == p2

instance Ord (Entry a) where
    Entry _ _ p1 <  Entry _ _ p2 = p1 <  p2
    Entry _ _ p1 <= Entry _ _ p2 = p1 <= p2

data PriorityQueue a = PriorityQueue {-# UNPACK #-} !Int (Heap (Entry a))

----------------------------------------------------------------

magicPriority :: Pri
magicPriority = 0

prioritySteps :: Int
prioritySteps = 65536

priorityList :: [Int]
priorityList = map calc idxs
  where
    idxs = [1..256] :: [Double]
    calc w = round (fromIntegral prioritySteps / w)

priorityTable :: Array Int Int
priorityTable = listArray (1,256) priorityList

weightToPriority :: Weight -> Pri
weightToPriority w = priorityTable ! w

----------------------------------------------------------------

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w magicPriority

renewEntry :: Entry a -> b -> Entry b
renewEntry (Entry _ w p) x = Entry x w p

isNewEntry :: Entry a -> Bool
isNewEntry (Entry _ _ p) = p == magicPriority

item :: Entry a -> a
item (Entry x _ _) = x

----------------------------------------------------------------

empty :: PriorityQueue a
empty = PriorityQueue 0 H.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty (PriorityQueue _ h) = H.null h

enqueue :: Entry a -> PriorityQueue a -> PriorityQueue a
enqueue ent@(Entry x w p) (PriorityQueue base heap) = PriorityQueue base heap'
  where
    !b = if isNewEntry ent then base else p
    !p' = b + weightToPriority w
    !ent' = Entry x w p'
    !heap' = H.insert ent' heap

dequeue :: PriorityQueue a -> Maybe (Entry a, PriorityQueue a)
dequeue (PriorityQueue _ heap) = case H.uncons heap of
    Nothing -> Nothing
    Just (ent@(Entry _ _ p), heap') -> let !base' = p `mod` prioritySteps
                                       in Just (ent, PriorityQueue base' heap')

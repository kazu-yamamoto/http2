{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Heap (
    Entry
  , newEntry
  , renewEntry
  , item
  , PriorityQueue
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
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64

-- | Abstract data type of entries for priority queues.
data Entry a = Entry {
    item :: a -- ^ Extracting an item from an entry.
  , weight  :: {-# UNPACK #-} !Weight
  , deficit :: {-# UNPACK #-} !Deficit
  } deriving Show

instance Eq (Entry a) where
    Entry _ _ p1 == Entry _ _ p2 = p1 == p2

instance Ord (Entry a) where
    Entry _ _ p1 <  Entry _ _ p2 = p1 <  p2
    Entry _ _ p1 <= Entry _ _ p2 = p1 <= p2

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue {-# UNPACK #-} !Deficit
                                     (Heap (Entry a, Key)) -- Key MUST be the second

----------------------------------------------------------------

magicDeficit :: Deficit
magicDeficit = 0

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

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w magicDeficit

-- | Changing the item of an entry.
renewEntry :: Entry a -> b -> Entry b
renewEntry (Entry _ w p) x = Entry x w p

----------------------------------------------------------------

empty :: PriorityQueue a
empty = PriorityQueue 0 H.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty (PriorityQueue _ h) = H.null h

enqueue :: Key -> Entry a -> PriorityQueue a -> PriorityQueue a
enqueue k Entry{..} (PriorityQueue base heap) = PriorityQueue base heap'
  where
    !b = if deficit == magicDeficit then base else deficit
    !deficit' = b + weightToDeficit weight
    !ent' = Entry item weight deficit'
    !heap' = H.insert (ent',k) heap

dequeue :: PriorityQueue a -> Maybe (Key, Entry a, PriorityQueue a)
dequeue (PriorityQueue _ heap) = case H.uncons heap of
    Nothing                     -> Nothing
    Just ((ent@Entry{..},k), heap')
      | H.null heap' -> Just (k, ent, empty) -- reset the deficit base
      | otherwise    -> Just (k, ent, PriorityQueue deficit heap')

delete :: PriorityQueue a -> Key -> PriorityQueue a
delete (PriorityQueue base heap) k = PriorityQueue base heap'
  where
    !heap' = H.filter (\xk -> snd xk /= k) heap

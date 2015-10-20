{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Priority.PSQ (
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
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as P
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64
type Heap = IntPSQ Deficit

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
data PriorityQueue a = PriorityQueue {-# UNPACK #-} !Deficit (Heap (Entry a))

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
empty = PriorityQueue 0 P.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty (PriorityQueue _ h) = P.null h

enqueue :: Key -> Entry a -> PriorityQueue a -> PriorityQueue a
enqueue k Entry{..} (PriorityQueue base heap) = PriorityQueue base heap'
  where
    !b = if deficit == magicDeficit then base else deficit
    !deficit' = b + weightToDeficit weight
    !ent' = Entry item weight deficit'
    !heap' = P.insert k deficit' ent' heap

dequeue :: PriorityQueue a -> Maybe (Key, Entry a, PriorityQueue a)
dequeue (PriorityQueue _ heap) = case P.minView heap of
    Nothing                     -> Nothing
    Just (k, deficit, ent, heap')
      | P.null heap' -> Just (k, ent, empty) -- reset the deficit base
      | otherwise    -> Just (k, ent, PriorityQueue deficit heap')

delete :: PriorityQueue a -> Key -> PriorityQueue a
delete (PriorityQueue _ heap) k = case P.findMin heap' of
    Nothing            -> empty
    Just (_,deficit,_) -> PriorityQueue deficit heap'
  where
    heap' = P.delete k heap

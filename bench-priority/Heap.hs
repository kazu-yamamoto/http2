{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Heap (
    PriorityQueue(..)
  , Precedence(..)
  , newPrecedence
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

data Precedence = Precedence {
    deficit    :: {-# UNPACK #-} !Deficit
  , weight     :: {-# UNPACK #-} !Weight
  -- stream dependency, used by the upper layer
  , dependency :: {-# UNPACK #-} !Key
  } deriving Show

-- | For test only
newPrecedence :: Weight -> Precedence
newPrecedence w = Precedence 0 w 0

instance Eq Precedence where
  Precedence d1 _ _ == Precedence d2 _ _ = d1 == d2

instance Ord Precedence where
  Precedence d1 _ _ <  Precedence d2 _ _ = d1 <  d2
  Precedence d1 _ _ <= Precedence d2 _ _ = d1 <= d2

data Entry a = Entry Key Precedence a

instance Eq (Entry a) where
    Entry _ p1 _ == Entry _ p2 _ = p1 == p2

instance Ord (Entry a) where
    Entry _ p1 _ <  Entry _ p2 _ = p1 <  p2
    Entry _ p1 _ <= Entry _ p2 _ = p1 <= p2

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue {
    baseDeficit :: {-# UNPACK #-} !Deficit
  , queue :: Heap (Entry a)
  }

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

empty :: PriorityQueue a
empty = PriorityQueue 0 H.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty (PriorityQueue _ h) = H.null h

enqueue :: Key -> Precedence -> a -> PriorityQueue a -> PriorityQueue a
enqueue k p v PriorityQueue{..} = PriorityQueue b queue'
  where
    !b = if deficit p == magicDeficit then baseDeficit else deficit p
    !deficit' = b + weightToDeficit (weight p)
    !p' = p { deficit = deficit' }
    !queue' = H.insert (Entry k p' v) queue

dequeue :: PriorityQueue a -> Maybe (Key, Precedence, a, PriorityQueue a)
dequeue (PriorityQueue _ heap) = case H.uncons heap of
    Nothing                     -> Nothing
    Just (Entry k p v, heap')
      | H.null heap' -> Just (k, p, v, empty) -- reset the deficit base
      | otherwise    -> Just (k, p, v, PriorityQueue (deficit p) heap')

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k (PriorityQueue base heap) = (mv, PriorityQueue base' heap')
  where
    !(h,heap') = H.partition  (\(Entry k' _ _) -> k' == k) heap
    mv = case H.viewMin h of
        Nothing               -> Nothing
        Just (Entry _ _ v, _) -> Just v
    base' = case H.viewMin heap of
        Nothing       -> base
        Just (Entry k' p _, _)
          | k == k'   -> deficit p
          | otherwise -> base

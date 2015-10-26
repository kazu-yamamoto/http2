{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Priority.PSQ (
    PriorityQueue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , delete
  , clear
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.Array (Array, listArray, (!))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as P
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64
type Heap a = IntPSQ Deficit (Weight, a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue {
    baseDeficit :: {-# UNPACK #-} !Deficit
  , deficitMap :: IntMap Deficit
  , queue :: Heap a
  }

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
empty = PriorityQueue 0 I.empty P.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty PriorityQueue{..} = P.null queue

enqueue :: Key -> Weight -> a -> PriorityQueue a -> PriorityQueue a
enqueue k w x PriorityQueue{..} =
    PriorityQueue baseDeficit deficitMap' queue'
  where
    !b = case I.lookup k deficitMap of
        Nothing      -> baseDeficit
        Just deficit -> deficit
    !deficit' = b + weightToDeficit w
    !deficitMap' = I.insert k deficit' deficitMap
    !queue' = P.insert k deficit' (w,x) queue

dequeue :: PriorityQueue a -> Maybe (Key, Weight, a, PriorityQueue a)
dequeue PriorityQueue{..} = case P.minView queue of
    Nothing           -> Nothing
    Just (k, deficit, (w,x), queue')
      | P.null queue' -> Just (k, w, x, empty)
      | otherwise     -> Just (k, w, x, PriorityQueue deficit deficitMap queue')

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k PriorityQueue{..} = case P.findMin queue' of
    Nothing            -> (mx, empty)
    Just (_,deficit,_) -> (mx, PriorityQueue deficit deficitMap queue')
  where
    !mx = case P.lookup k queue of
        Nothing        -> Nothing
        Just (_,(_,x)) -> Just x
    !queue' = P.delete k queue

clear :: Key -> PriorityQueue a -> PriorityQueue a
clear k PriorityQueue{..} = PriorityQueue baseDeficit deficitMap' queue'
  where
    !deficitMap' = I.delete k deficitMap
    !queue' = P.delete k queue -- just in case

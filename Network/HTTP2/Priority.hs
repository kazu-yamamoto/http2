{-# LANGUAGE CPP #-}

-- | This is partial implementation of the priority of HTTP/2.
--
-- This implementation does support structured priority queue
-- but not support re-structuring. This means that it is assumed that
-- an entry created by a Priority frame is never closed. The entry
-- behaves an intermediate node, not a leaf.
--
-- This queue is fair for weight. Consider two weights: 201 and 101.
-- Repeating enqueue/dequeue probably produces
-- 201, 201, 101, 201, 201, 101, ... based on randomness.
--
-- Only one entry per stream should be enqueued.
-- If multiple entries for a stream are inserted, the ordering
-- is not preserved because of the randomness.

module Network.HTTP2.Priority (
    PriorityTree
  , newPriorityTree
  , prepare
  , enqueue
  , dequeue
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent.STM
import Control.Monad (when, unless)
import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap)
import Network.HTTP2.RandomSkewHeap (Heap)
import qualified Network.HTTP2.RandomSkewHeap as Heap
import Network.HTTP2.Types

----------------------------------------------------------------

type Struct a = (PriorityQueue a, Priority)
-- | Abstract data type for priority trees.
data PriorityTree a = PriorityTree (TVar (IntMap (Struct a)))
                                   (PriorityQueue a)
-- INVARIANT: Empty PriorityQueue is never enqueued in
-- another PriorityQueue.
type PriorityQueue a = TPQueue (Element a)
data Element a = Child a
               | Parent (PriorityQueue a)

-- | Creating a new priority tree.
newPriorityTree :: IO (PriorityTree a)
newPriorityTree = PriorityTree <$> newTVarIO Map.empty <*> atomically newTPQueue

newPriorityQueue :: STM (PriorityQueue a)
newPriorityQueue = TPQueue <$> newTVar Heap.empty

----------------------------------------------------------------

-- | Bringing up the structure of the priority tree.
--   This must be used for Priority frame.
prepare :: PriorityTree a -> StreamId -> Priority -> IO ()
prepare (PriorityTree var _) sid p = atomically $ do
    q <- newPriorityQueue
    modifyTVar' var $ Map.insert sid (q, p)

-- | Enqueuing an element to the priority tree.
--   This must be used for Header frame.
enqueue :: PriorityTree a -> a -> Priority -> IO ()
enqueue (PriorityTree var q0) a p0 = atomically $ do
    m <- readTVar var
    loop m (Child a) p0
  where
    loop m el p
      | pid == 0  = writeTPQueue q0 el p
      | otherwise = case Map.lookup pid m of
          Nothing -> writeTPQueue q0 el defaultPriority -- error case: checkme
          Just (q', p') -> do
              notQueued <- isTPQueueEmpty q'
              writeTPQueue q' el p
              when notQueued $ loop m (Parent q') p'
      where
        pid = streamDependency p

-- | Dequeuing an element from the priority tree.
dequeue :: PriorityTree a -> IO (a, Priority)
dequeue (PriorityTree _ q0) = atomically (loop q0)
  where
    loop q = do
        (el, w) <- readTPQueue q
        case el of
            Child  a      -> return (a, w)
            p@(Parent q') -> do
                r <- loop q'
                empty <- isTPQueueEmpty q'
                unless empty $ writeTPQueue q p w
                return r

----------------------------------------------------------------
--
-- The following code is originally written by Fumiaki Kinoshita
--

newtype TPQueue a = TPQueue (TVar (Heap (a,Priority)))

newTPQueue :: STM (TPQueue a)
newTPQueue = TPQueue <$> newTVar Heap.empty

readTPQueue :: TPQueue a -> STM (a, Priority)
readTPQueue (TPQueue th) = do
  h <- readTVar th
  case Heap.uncons h of
    Nothing -> retry
    Just (ap, _, h') -> do
      writeTVar th h'
      return ap

writeTPQueue :: TPQueue a -> a -> Priority -> STM ()
writeTPQueue (TPQueue th) a p = modifyTVar' th $ Heap.insert (a,p) (weight p)

isTPQueueEmpty :: TPQueue a -> STM Bool
isTPQueueEmpty (TPQueue th) = Heap.isEmpty <$> readTVar th

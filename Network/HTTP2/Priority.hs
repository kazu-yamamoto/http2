{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | This is partial implementation of the priority of HTTP/2.
--
-- This implementation does support structured priority queue
-- but not support re-structuring. This means that it is assumed that
-- an entry created by a Priority frame is never closed. The entry
-- behaves an intermediate node, not a leaf.
--
-- This queue is fair for weight. Consider two weights: 201 and 101.
-- Repeating enqueue/dequeue probably produces
-- 201, 201, 101, 201, 201, 101, ...
--
-- Only one entry per stream should be enqueued.

module Network.HTTP2.Priority (
  -- * Entry
    Entry
  , newEntry
  , Q.renewEntry
  , Q.item
  -- * PriorityTree
  , PriorityTree
  , newPriorityTree
  -- * PriorityTree functions
  , prepare
  , enqueue
  , dequeue
  , delete
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent.STM
import Control.Monad (when, unless)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Network.HTTP2.Priority.Queue (TPriorityQueue, Entry)
import qualified Network.HTTP2.Priority.Queue as Q
import Network.HTTP2.Types

----------------------------------------------------------------

-- | Wrapping an item to an entry.
newEntry :: a -> Priority -> Entry a
newEntry x p = Q.newEntry x (weight p)

----------------------------------------------------------------

-- | Abstract data type for priority trees.
data PriorityTree a = PriorityTree (TVar (Glue a))
                                   (TNestedPriorityQueue a)

type Glue a = IntMap (TNestedPriorityQueue a, Priority)

-- INVARIANT: Empty TNestedPriorityQueue is never enqueued in
-- another TNestedPriorityQueue.
type TNestedPriorityQueue a = TPriorityQueue (Element a)

data Element a = Child a
               | Parent (TNestedPriorityQueue a)

----------------------------------------------------------------

-- | Creating a new priority tree.
newPriorityTree :: IO (PriorityTree a)
newPriorityTree = PriorityTree <$> newTVarIO Map.empty <*> atomically Q.new

----------------------------------------------------------------

-- | Bringing up the structure of the priority tree.
--   This must be used for Priority frame.
prepare :: PriorityTree a -> StreamId -> Priority -> IO ()
prepare (PriorityTree var _) sid p = atomically $ do
    q <- Q.new
    modifyTVar' var $ Map.insert sid (q, p)

-- | Enqueuing an entry to the priority tree.
--   This must be used for Header frame.
enqueue :: PriorityTree a -> StreamId -> Priority -> Entry a -> IO ()
enqueue (PriorityTree var q0) sid p0 ent0 = atomically $ do
    m <- readTVar var
    let !x = Q.item ent0
        !el = Child x
        !ent' = Q.renewEntry ent0 el
    loop m ent' p0
  where
    loop m ent p
      | pid == 0  = Q.enqueue q0 sid ent
      | otherwise = case Map.lookup pid m of
          Nothing -> Q.enqueue q0 sid ent -- error case: checkme
          Just (q', p') -> do
              notQueued <- Q.isEmpty q'
              Q.enqueue q' sid ent
              when notQueued $ do
                  let !ent' = newEntry (Parent q') p'
                  loop m ent' p'
      where
        pid = streamDependency p

-- | Dequeuing an entry from the priority tree.
dequeue :: PriorityTree a -> IO (StreamId, Entry a)
dequeue (PriorityTree _ q0) = atomically (loop q0)
  where
    loop q = do
        (sid,ent) <- Q.dequeue q
        case Q.item ent of
            Child x   -> return $! (sid, Q.renewEntry ent x)
            Parent q' -> do
                entr <- loop q'
                empty <- Q.isEmpty q'
                unless empty $ Q.enqueue q sid ent
                return entr

-- | Deleting the entry corresponding to 'StreamId'.
--   'delete' and 'enqueue' are used to change the priority of
--   a live stream.
delete :: PriorityTree a -> StreamId -> IO ()
delete (PriorityTree _ q) sid = atomically $ Q.delete q sid

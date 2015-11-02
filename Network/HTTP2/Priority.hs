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
  -- * PriorityTree
    PriorityTree
  , newPriorityTree
  -- * PriorityTree functions
  , prepare
  , enqueue
  , dequeue
  , delete
  , clear
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent.STM
import Control.Monad (when, unless)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Network.HTTP2.Priority.Queue (TPriorityQueue)
import qualified Network.HTTP2.Priority.Queue as Q
import Network.HTTP2.Types

----------------------------------------------------------------

-- | Abstract data type for priority trees.
data PriorityTree a = PriorityTree (TVar (Glue a))
                                   (TNestedPriorityQueue a)
                                   (TQueue (StreamId, a))

type Glue a = IntMap (TNestedPriorityQueue a, Priority)

-- INVARIANT: Empty TNestedPriorityQueue is never enqueued in
-- another TNestedPriorityQueue.
type TNestedPriorityQueue a = TPriorityQueue (Element a)

data Element a = Child a
               | Parent (TNestedPriorityQueue a)

----------------------------------------------------------------

-- | Creating a new priority tree.
newPriorityTree :: IO (PriorityTree a)
newPriorityTree = PriorityTree <$> newTVarIO Map.empty
                               <*> atomically Q.new
                               <*> newTQueueIO

----------------------------------------------------------------

-- | Bringing up the structure of the priority tree.
--   This must be used for Priority frame.
prepare :: PriorityTree a -> StreamId -> Priority -> IO ()
prepare (PriorityTree var _ _) sid p = atomically $ do
    q <- Q.new
    modifyTVar' var $ Map.insert sid (q, p)

-- | Enqueuing an entry to the priority tree.
--   This must be used for Header frame.
--   If 'controlPriority' is specified,
--   it is treated as a control frame and top-queued.
enqueue :: PriorityTree a -> StreamId -> Priority -> a -> IO ()
enqueue (PriorityTree _ _ cq) sid p0 x
  | p0 == controlPriority = atomically $ writeTQueue cq (sid,x)
enqueue (PriorityTree var q0 _) sid p0 x = atomically $ do
    m <- readTVar var
    let !el = Child x
    loop m el p0
  where
    loop m el p
      | pid == 0  = Q.enqueue q0 sid w el
      | otherwise = case Map.lookup pid m of
          -- If not found, enqueuing it to the stream 0 queue.
          Nothing -> Q.enqueue q0 sid w el
          Just (q', p') -> do
              notQueued <- Q.isEmpty q'
              Q.enqueue q' sid w el
              when notQueued $ do
                  let !el' = Parent q'
                  loop m el' p'
      where
        pid = streamDependency p
        w   = weight p

-- | Dequeuing an entry from the priority tree.
dequeue :: PriorityTree a -> IO (StreamId, a)
dequeue (PriorityTree _ q0 cq) = atomically $ do
    mx <- tryReadTQueue cq
    case mx of
        Just x  -> return x
        Nothing -> loop q0
  where
    loop q = do
        (sid,w,el) <- Q.dequeue q
        case el of
            Child x   -> return $! (sid, x)
            Parent q' -> do
                entr <- loop q'
                empty <- Q.isEmpty q'
                unless empty $ Q.enqueue q sid w el
                return entr

-- | Deleting the entry corresponding to 'StreamId'.
--   'delete' and 'enqueue' are used to change the priority of
--   a live stream.
delete :: PriorityTree a -> StreamId -> Priority -> IO (Maybe a)
delete (PriorityTree var q0 _) sid p
  | pid == 0  = atomically $ del q0
  | otherwise = atomically $ do
        m <- readTVar var
        case Map.lookup pid m of
            Nothing    -> return Nothing
            Just (q,_) -> del q
  where
    pid = streamDependency p
    del q = do
        mel <- Q.delete sid q
        case mel of
            Nothing -> return Nothing
            Just el -> case el of
                Child  x -> return $ Just x
                Parent _ -> return Nothing -- fixme: this is error

-- | Clearing the internal state for 'StreamId' from 'PriorityTree'.
--   When a stream is closed, this function MUST be called
--   to prevent memory leak.
clear :: PriorityTree a -> StreamId -> Priority -> IO ()
clear (PriorityTree var q0 _) sid p
  | pid == 0  = atomically $ Q.clear sid q0
  | otherwise = atomically $ do
        m <- readTVar var
        case Map.lookup pid m of
            Nothing    -> return ()
            Just (q,_) -> Q.clear sid q
  where
    pid = streamDependency p

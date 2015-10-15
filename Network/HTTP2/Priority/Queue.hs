module Network.HTTP2.Priority.Queue (
    TPriorityQueue
  , new
  , isEmpty
  , enqueue
  , dequeue
  ) where

import Control.Concurrent.STM
import Network.HTTP2.Priority.RandomSkewHeap (PriorityQueue)
import qualified Network.HTTP2.Priority.RandomSkewHeap as Q
import Network.HTTP2.Types (Priority(..))

----------------------------------------------------------------
--
-- The following code is originally written by Fumiaki Kinoshita
--

newtype TPriorityQueue a = TPriorityQueue (TVar (PriorityQueue (a,Priority)))

new :: STM (TPriorityQueue a)
new = TPriorityQueue <$> newTVar Q.empty

isEmpty :: TPriorityQueue a -> STM Bool
isEmpty (TPriorityQueue th) = Q.isEmpty <$> readTVar th

enqueue :: TPriorityQueue a -> a -> Priority -> STM ()
enqueue (TPriorityQueue th) a p = modifyTVar' th $ Q.enqueue (a,p) (weight p)

dequeue :: TPriorityQueue a -> STM (a, Priority)
dequeue (TPriorityQueue th) = do
  h <- readTVar th
  case Q.dequeue h of
    Nothing -> retry
    Just (ap, _, h') -> do
      writeTVar th h'
      return ap

module Network.HTTP2.Priority.Queue (
    Entry
  , Q.newEntry
  , Q.renewEntry
  , Q.item
  , TPriorityQueue
  , new
  , isEmpty
  , enqueue
  , dequeue
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Concurrent.STM
import Network.HTTP2.Priority.Heap (PriorityQueue, Entry)
import qualified Network.HTTP2.Priority.Heap as Q

----------------------------------------------------------------

newtype TPriorityQueue a = TPriorityQueue (TVar (PriorityQueue a))

new :: STM (TPriorityQueue a)
new = TPriorityQueue <$> newTVar Q.empty

isEmpty :: TPriorityQueue a -> STM Bool
isEmpty (TPriorityQueue th) = Q.isEmpty <$> readTVar th

enqueue :: TPriorityQueue a -> Entry a -> STM ()
enqueue (TPriorityQueue th) ent = modifyTVar' th $ Q.enqueue ent

dequeue :: TPriorityQueue a -> STM (Entry a)
dequeue (TPriorityQueue th) = do
  h <- readTVar th
  case Q.dequeue h of
    Nothing -> retry
    Just (ent, h') -> do
      writeTVar th h'
      return ent

{-# LANGUAGE CPP #-}

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
  , delete
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Concurrent.STM
import Network.HTTP2.Priority.PSQ (PriorityQueue, Entry)
import qualified Network.HTTP2.Priority.PSQ as Q

----------------------------------------------------------------

type Key = Int

newtype TPriorityQueue a = TPriorityQueue (TVar (PriorityQueue a))

new :: STM (TPriorityQueue a)
new = TPriorityQueue <$> newTVar Q.empty

isEmpty :: TPriorityQueue a -> STM Bool
isEmpty (TPriorityQueue th) = Q.isEmpty <$> readTVar th

enqueue :: TPriorityQueue a -> Key -> Entry a -> STM ()
enqueue (TPriorityQueue th) key ent = modifyTVar' th $ Q.enqueue key ent

dequeue :: TPriorityQueue a -> STM (Key, Entry a)
dequeue (TPriorityQueue th) = do
  h <- readTVar th
  case Q.dequeue h of
    Nothing -> retry
    Just (key, ent, h') -> do
      writeTVar th h'
      return (key, ent)

delete :: TPriorityQueue a -> Key -> STM ()
delete (TPriorityQueue th) k = modifyTVar' th $ \q -> Q.delete q k

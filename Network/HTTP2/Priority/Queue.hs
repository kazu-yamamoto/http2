{-# LANGUAGE CPP #-}

module Network.HTTP2.Priority.Queue (
    TPriorityQueue
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
import Network.HTTP2.Priority.PSQ (PriorityQueue)
import qualified Network.HTTP2.Priority.PSQ as Q

----------------------------------------------------------------

type Key = Int
type Weight = Int

newtype TPriorityQueue a = TPriorityQueue (TVar (PriorityQueue a))

new :: STM (TPriorityQueue a)
new = TPriorityQueue <$> newTVar Q.empty

isEmpty :: TPriorityQueue a -> STM Bool
isEmpty (TPriorityQueue th) = Q.isEmpty <$> readTVar th

enqueue :: TPriorityQueue a -> Key -> Weight -> a -> STM ()
enqueue (TPriorityQueue th) k w x = modifyTVar' th $ Q.enqueue k w x

dequeue :: TPriorityQueue a -> STM (Key, Weight, a)
dequeue (TPriorityQueue th) = do
  h <- readTVar th
  case Q.dequeue h of
    Nothing -> retry
    Just (k, w, x, h') -> do
      writeTVar th h'
      return (k, w, x)

delete :: Key -> TPriorityQueue a -> STM ()
delete k (TPriorityQueue th) = modifyTVar' th $ \q -> Q.delete k q

{-# LANGUAGE RecordWildCards #-}

module DoublyLinkedQueueIO (
    Queue
  , Node
  , item
  , new
  , isEmpty
  , enqueue
  , dequeue
  , delete
  ) where

import Data.IORef

data Queue a = Queue {
    entr :: Node a
  , exit :: Node a
  }

data Node a = Node {
    item :: a
  , prev :: {-# UNPACK #-} !(IORef (Node a))
  , next :: {-# UNPACK #-} !(IORef (Node a))
  } deriving Eq

newNode :: a -> IO (Node a)
newNode x = Node x <$> newIORef undefined <*> newIORef undefined

{-# INLINE getNext #-}
getNext :: Node a -> IO (Node a)
getNext Node{..} = readIORef next

{-# INLINE setNext #-}
setNext :: Node a -> Node a -> IO ()
setNext Node{..} x = writeIORef next x

{-# INLINE getPrev #-}
getPrev :: Node a -> IO (Node a)
getPrev Node{..} = readIORef prev

{-# INLINE setPrev #-}
setPrev :: Node a -> Node a -> IO ()
setPrev Node{..} x = writeIORef prev x

new :: IO (Queue a)
new = do
    a1 <- newNode undefined
    a2 <- newNode undefined
    setPrev a1 a2
    setNext a1 a2
    setPrev a2 a1
    setNext a2 a1
    return $! Queue a1 a2

isEmpty :: Queue a -> IO Bool
isEmpty Queue{..} = do
    n <- getNext entr
    nn <- getNext n
    return $! next entr == next nn

enqueue :: a -> Queue a -> IO (Node a)
enqueue a Queue{..} = do
    x <- newNode a
    n <- getNext entr
    setPrev x entr
    setNext x n
    setPrev n x
    setNext entr x
    return x

dequeue :: Queue a -> IO a
dequeue Queue{..} = do
    p <- getPrev exit
    pp <- getPrev p
    setPrev exit pp
    setNext pp exit
    return $! item p

delete :: Node a -> IO ()
delete x = do
    p <- getPrev x
    n <- getNext x
    setNext p n
    setPrev n p

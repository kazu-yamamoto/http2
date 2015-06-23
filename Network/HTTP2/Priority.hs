{-# LANGUAGE RecordWildCards, CPP #-}

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

prepare :: PriorityTree a -> StreamId -> Priority -> IO ()
prepare (PriorityTree var _) sid Priority{..} = atomically $ do
    q <- newPriorityQueue
    let pid = fromStreamIdentifier streamDependency
    modifyTVar' var $ Map.insert sid (q, weight, pid)

enqueue :: PriorityTree a -> a -> Priority -> IO ()
enqueue (PriorityTree var q0) a Priority{..} = atomically $ do
    m <- readTVar var
    let pid = fromStreamIdentifier streamDependency
        el = Child a
    loop m el weight pid
  where
    loop _ el w 0 = writeTPQueue q0 el w
    loop m el w pid = do
        case Map.lookup pid m of
            Nothing -> writeTPQueue q0 el 16
            Just (q', w', pid') -> do
                exist <- nonEmpty q'
                writeTPQueue q' el w
                unless exist $ loop m (Parent q') w' pid'

dequeue :: PriorityTree a -> IO (a, Weight)
dequeue (PriorityTree _ q0) = atomically (loop q0)
  where
    loop q = do
        (el, w) <- readTPQueue q
        case el of
            Child  a      -> return (a, w)
            p@(Parent q') -> do
                r <- loop q'
                x <- nonEmpty q'
                when x $ writeTPQueue q p w
                return r

----------------------------------------------------------------

type Struct a = (PriorityQueue a, Weight, StreamId)
data PriorityTree a = PriorityTree (TVar (IntMap (Struct a)))
                                     (PriorityQueue a)
type PriorityQueue a = TPQueue (Element a)
data Element a = Child a
               | Parent (PriorityQueue a)

newPriorityTree :: IO (PriorityTree a)
newPriorityTree = do
    ref <- newTVarIO Map.empty
    q <- atomically newTPQueue
    return $ PriorityTree ref q

newPriorityQueue :: STM (PriorityQueue a)
newPriorityQueue = TPQueue <$> newTVar Heap.empty

----------------------------------------------------------------
--
-- The following code is originally written by Fumiaki Kinoshita
--

newtype TPQueue a = TPQueue (TVar (Heap a))

newTPQueue :: STM (TPQueue a)
newTPQueue = TPQueue <$> newTVar Heap.empty

readTPQueue :: TPQueue a -> STM (a, Weight)
readTPQueue (TPQueue th) = do
  h <- readTVar th
  case Heap.uncons h of
    Nothing -> retry
    Just (a, w, h') -> do
      writeTVar th h'
      return (a, w)

tryReadTPQueue :: TPQueue a -> STM (Maybe (a, Weight))
tryReadTPQueue (TPQueue th) = do
  h <- readTVar th
  case Heap.uncons h of
    Nothing -> return Nothing
    Just (a, w, h') -> do
      writeTVar th h'
      return (Just (a, w))

writeTPQueue :: TPQueue a -> a -> Weight -> STM ()
writeTPQueue (TPQueue th) a w = modifyTVar' th $ Heap.insert a w

nonEmpty :: TPQueue a -> STM Bool
nonEmpty (TPQueue th) = Heap.isEmpty <$> readTVar th

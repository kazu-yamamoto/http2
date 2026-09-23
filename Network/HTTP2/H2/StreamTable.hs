{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.StreamTable (
    -- * Types
    OddStreamTable (..),
    emptyOddStreamTable,
    EvenStreamTable (..),
    emptyEvenStreamTable,

    -- * Odd
    insertOdd,
    insertOdd',
    deleteOdd,
    lookupOdd,
    getOddConcurrency,
    getOddStreams,
    clearOddStreamTable,
    waitIncOdd,

    -- * Even
    insertEven,
    insertEven',
    deleteEven,
    lookupEven,
    getEvenConcurrency,
    clearEvenStreamTable,
    waitIncEven,
    insertEvenCache,
    deleteEvenCache,
    lookupEvenCache,
    getEvenStreams,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Network.Control (LRUCache)
import qualified Network.Control as LRUCache

import Imports
import Network.HTTP2.H2.Types (Stream (..))

----------------------------------------------------------------

data OddStreamTable = OddStreamTable
    { oddConc :: Int
    , oddTable :: IntMap Stream
    }

emptyOddStreamTable :: OddStreamTable
emptyOddStreamTable = OddStreamTable 0 IntMap.empty

data EvenStreamTable = EvenStreamTable
    { evenConc :: Int
    , evenTable :: IntMap Stream
    , -- Cache must contain Stream instead of StreamId because
      -- a Stream is deleted when end-of-stream is received.
      -- After that, cache is looked up.
      -- LRUCache is not used as LRU but as fixed-size map.
      evenCache :: LRUCache (Method, ByteString) Stream
    }

emptyEvenStreamTable :: Int -> EvenStreamTable
emptyEvenStreamTable lim = EvenStreamTable 0 IntMap.empty $ LRUCache.empty lim

----------------------------------------------------------------

insertOdd :: TVar OddStreamTable -> IntMap.Key -> Stream -> IO ()
insertOdd var k v = atomically $ modifyTVar var $ \OddStreamTable{..} ->
    let oddConc' = oddConc + 1
        oddTable' = IntMap.insert k v oddTable
     in OddStreamTable oddConc' oddTable'

insertOdd' :: TVar OddStreamTable -> IntMap.Key -> Stream -> IO ()
insertOdd' var k v = atomically $ modifyTVar var $ \OddStreamTable{..} ->
    let oddTable' = IntMap.insert k v oddTable
     in OddStreamTable oddConc oddTable'

-- | Remove a stream and give its concurrency slot back.
--
-- 'closed' can be called more than once for the same stream -- a RST_STREAM
-- carrying a non-critical error code goes through both 'stream' and
-- 'processState', each of which closes it -- so the count must follow an
-- entry that was really there, not the number of calls.
deleteOdd :: TVar OddStreamTable -> IntMap.Key -> E.SomeException -> IO ()
deleteOdd var k err = do
    mv <- atomically deleteStream
    case mv of
        Nothing -> return () -- Stream was already removed
        Just v -> void . tryPutMVar (streamInput v) $ Left err
  where
    deleteStream :: STM (Maybe Stream)
    deleteStream = do
        OddStreamTable{..} <- readTVar var
        case IntMap.lookup k oddTable of
            Nothing -> return Nothing
            Just v -> do
                let oddConc' = oddConc - 1
                    oddTable' = IntMap.delete k oddTable
                writeTVar var $ OddStreamTable oddConc' oddTable'
                return $ Just v

lookupOdd :: TVar OddStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupOdd var k = IntMap.lookup k . oddTable <$> readTVarIO var

getOddConcurrency :: TVar OddStreamTable -> IO Int
getOddConcurrency var = oddConc <$> readTVarIO var

getOddStreams :: TVar OddStreamTable -> IO (IntMap Stream)
getOddStreams var = oddTable <$> readTVarIO var

clearOddStreamTable :: TVar OddStreamTable -> IO (IntMap Stream)
clearOddStreamTable var = atomically $ do
    OddStreamTable{..} <- readTVar var
    writeTVar var emptyOddStreamTable
    return oddTable

waitIncOdd :: TVar OddStreamTable -> Int -> STM ()
waitIncOdd var maxConc = do
    OddStreamTable{..} <- readTVar var
    check (oddConc < maxConc)
    let oddConc' = oddConc + 1
    writeTVar var $ OddStreamTable oddConc' oddTable

----------------------------------------------------------------

insertEven :: TVar EvenStreamTable -> IntMap.Key -> Stream -> IO ()
insertEven var k v = atomically $ modifyTVar var $ \EvenStreamTable{..} ->
    let evenConc' = evenConc + 1
        evenTable' = IntMap.insert k v evenTable
     in EvenStreamTable evenConc' evenTable' evenCache

insertEven' :: TVar EvenStreamTable -> IntMap.Key -> Stream -> IO ()
insertEven' var k v = atomically $ modifyTVar var $ \EvenStreamTable{..} ->
    let evenTable' = IntMap.insert k v evenTable
     in EvenStreamTable evenConc evenTable' evenCache

-- | Remove a stream and give its concurrency slot back.
-- Idempotent, for the same reason as 'deleteOdd'.
deleteEven :: TVar EvenStreamTable -> IntMap.Key -> E.SomeException -> IO ()
deleteEven var k err = do
    mv <- atomically deleteStream
    case mv of
        Nothing -> return () -- Stream was already removed
        Just v -> void . tryPutMVar (streamInput v) $ Left err
  where
    deleteStream :: STM (Maybe Stream)
    deleteStream = do
        EvenStreamTable{..} <- readTVar var
        case IntMap.lookup k evenTable of
            Nothing -> return Nothing
            Just v -> do
                let evenConc' = evenConc - 1
                    evenTable' = IntMap.delete k evenTable
                writeTVar var $ EvenStreamTable evenConc' evenTable' evenCache
                return $ Just v

lookupEven :: TVar EvenStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupEven var k = IntMap.lookup k . evenTable <$> readTVarIO var

getEvenConcurrency :: TVar EvenStreamTable -> IO Int
getEvenConcurrency var = evenConc <$> readTVarIO var

clearEvenStreamTable :: TVar EvenStreamTable -> IO (IntMap Stream)
clearEvenStreamTable var = atomically $ do
    EvenStreamTable{..} <- readTVar var
    writeTVar var $ emptyEvenStreamTable 0
    return evenTable

waitIncEven :: TVar EvenStreamTable -> Int -> STM ()
waitIncEven var maxConc = do
    EvenStreamTable{..} <- readTVar var
    check (evenConc < maxConc)
    let evenConc' = evenConc + 1
    writeTVar var $ EvenStreamTable evenConc' evenTable evenCache

insertEvenCache
    :: TVar EvenStreamTable -> Method -> ByteString -> Stream -> IO ()
insertEvenCache var method path strm@Stream{..} = atomically $ modifyTVar var $ \EvenStreamTable{..} ->
    let evenConc' = evenConc + 1
        evenTable' = IntMap.insert streamNumber strm evenTable
        evenCache' = LRUCache.insert (method, path) strm evenCache
     in EvenStreamTable evenConc' evenTable' evenCache'

deleteEvenCache :: TVar EvenStreamTable -> Method -> ByteString -> IO ()
deleteEvenCache var m path = atomically $ modifyTVar var $ \EvenStreamTable{..} ->
    let evenCache' = LRUCache.delete (m, path) evenCache
     in EvenStreamTable evenConc evenTable evenCache'

lookupEvenCache
    :: TVar EvenStreamTable -> Method -> ByteString -> IO (Maybe Stream)
lookupEvenCache var m path = LRUCache.lookup (m, path) . evenCache <$> readTVarIO var

getEvenStreams :: TVar EvenStreamTable -> IO (IntMap Stream)
getEvenStreams var = evenTable <$> readTVarIO var

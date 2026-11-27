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
    refuseIfOddClosed,
    waitOddClosed,

    -- * Even
    insertEven,
    insertEven',
    deleteEven,
    lookupEven,
    getEvenConcurrency,
    clearEvenStreamTable,
    waitIncEven,
    isEvenClosed,
    insertEvenCache,
    deleteEvenCache,
    lookupEvenCache,
    getEvenStreams,
) where

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
    , oddClosed :: Maybe E.SomeException
    -- ^ Why the connection ended, once it has: no stream is added then,
    --   as nothing would ever finish it.
    }

emptyOddStreamTable :: OddStreamTable
emptyOddStreamTable = OddStreamTable 0 IntMap.empty Nothing

data EvenStreamTable = EvenStreamTable
    { evenConc :: Int
    , evenTable :: IntMap Stream
    , -- Cache must contain Stream instead of StreamId because
      -- a Stream is deleted when end-of-stream is received.
      -- After that, cache is looked up.
      -- LRUCache is not used as LRU but as fixed-size map.
      evenCache :: LRUCache (Method, ByteString) Stream
    , evenClosed :: Maybe E.SomeException
    -- ^ As 'oddClosed'.
    }

emptyEvenStreamTable :: Int -> EvenStreamTable
emptyEvenStreamTable lim = EvenStreamTable 0 IntMap.empty (LRUCache.empty lim) Nothing

----------------------------------------------------------------

-- | Throwing why the connection ended, once it has.
insertOdd :: TVar OddStreamTable -> IntMap.Key -> Stream -> IO ()
insertOdd var k v = atomically $ do
    refuseIfOddClosed var
    modifyTVar var $ \st@OddStreamTable{..} ->
        st{oddConc = oddConc + 1, oddTable = IntMap.insert k v oddTable}

-- | 'insertOdd' for a stream already counted by 'waitIncOdd'.
insertOdd' :: TVar OddStreamTable -> IntMap.Key -> Stream -> IO ()
insertOdd' var k v = atomically $ do
    refuseIfOddClosed var
    modifyTVar var $ \st@OddStreamTable{..} ->
        st{oddTable = IntMap.insert k v oddTable}

-- | Throwing why the connection ended, once it has; nothing otherwise.
refuseIfOddClosed :: TVar OddStreamTable -> STM ()
refuseIfOddClosed var = readTVar var >>= mapM_ throwSTM . oddClosed

-- | Waiting until the connection has ended, and throwing why.
waitOddClosed :: TVar OddStreamTable -> STM a
waitOddClosed var = readTVar var >>= maybe retry throwSTM . oddClosed

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
        Just v -> void . atomically . tryPutTMVar (streamInput v) $ Left err
  where
    deleteStream :: STM (Maybe Stream)
    deleteStream = do
        OddStreamTable{..} <- readTVar var
        case IntMap.lookup k oddTable of
            Nothing -> return Nothing
            Just v -> do
                writeTVar var $
                    OddStreamTable (oddConc - 1) (IntMap.delete k oddTable) oddClosed
                return $ Just v

lookupOdd :: TVar OddStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupOdd var k = IntMap.lookup k . oddTable <$> readTVarIO var

getOddConcurrency :: TVar OddStreamTable -> IO Int
getOddConcurrency var = oddConc <$> readTVarIO var

getOddStreams :: TVar OddStreamTable -> IO (IntMap Stream)
getOddStreams var = oddTable <$> readTVarIO var

-- | Emptying the table for good: the first reason given is kept.
clearOddStreamTable
    :: TVar OddStreamTable -> E.SomeException -> IO (IntMap Stream)
clearOddStreamTable var err = atomically $ do
    OddStreamTable{..} <- readTVar var
    writeTVar var $ OddStreamTable 0 IntMap.empty (oddClosed <|> Just err)
    return oddTable

-- | Throwing why the connection ended, once it has, rather than waiting.
waitIncOdd :: TVar OddStreamTable -> Int -> STM ()
waitIncOdd var maxConc = do
    refuseIfOddClosed var
    st@OddStreamTable{..} <- readTVar var
    check (oddConc < maxConc)
    writeTVar var st{oddConc = oddConc + 1}

----------------------------------------------------------------

-- | Throwing why the connection ended, once it has.
insertEven :: TVar EvenStreamTable -> IntMap.Key -> Stream -> IO ()
insertEven var k v = atomically $ do
    refuseIfEvenClosed var
    modifyTVar var $ \st@EvenStreamTable{..} ->
        st{evenConc = evenConc + 1, evenTable = IntMap.insert k v evenTable}

-- | 'insertEven' for a stream already counted by 'waitIncEven'.
insertEven' :: TVar EvenStreamTable -> IntMap.Key -> Stream -> IO ()
insertEven' var k v = atomically $ do
    refuseIfEvenClosed var
    modifyTVar var $ \st@EvenStreamTable{..} ->
        st{evenTable = IntMap.insert k v evenTable}

refuseIfEvenClosed :: TVar EvenStreamTable -> STM ()
refuseIfEvenClosed var = readTVar var >>= mapM_ throwSTM . evenClosed

isEvenClosed :: TVar EvenStreamTable -> STM Bool
isEvenClosed var = isJust . evenClosed <$> readTVar var

-- | Remove a stream and give its concurrency slot back.
-- Idempotent, for the same reason as 'deleteOdd'.
deleteEven :: TVar EvenStreamTable -> IntMap.Key -> E.SomeException -> IO ()
deleteEven var k err = do
    mv <- atomically deleteStream
    case mv of
        Nothing -> return () -- Stream was already removed
        Just v -> void . atomically . tryPutTMVar (streamInput v) $ Left err
  where
    deleteStream :: STM (Maybe Stream)
    deleteStream = do
        EvenStreamTable{..} <- readTVar var
        case IntMap.lookup k evenTable of
            Nothing -> return Nothing
            Just v -> do
                writeTVar var $
                    EvenStreamTable (evenConc - 1) (IntMap.delete k evenTable) evenCache evenClosed
                return $ Just v

lookupEven :: TVar EvenStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupEven var k = IntMap.lookup k . evenTable <$> readTVarIO var

getEvenConcurrency :: TVar EvenStreamTable -> IO Int
getEvenConcurrency var = evenConc <$> readTVarIO var

-- | As 'clearOddStreamTable'.
clearEvenStreamTable
    :: TVar EvenStreamTable -> E.SomeException -> IO (IntMap Stream)
clearEvenStreamTable var err = atomically $ do
    EvenStreamTable{..} <- readTVar var
    writeTVar var $
        EvenStreamTable 0 IntMap.empty (LRUCache.empty 0) (evenClosed <|> Just err)
    return evenTable

-- | As 'waitIncOdd'.
waitIncEven :: TVar EvenStreamTable -> Int -> STM ()
waitIncEven var maxConc = do
    refuseIfEvenClosed var
    st@EvenStreamTable{..} <- readTVar var
    check (evenConc < maxConc)
    writeTVar var st{evenConc = evenConc + 1}

insertEvenCache
    :: TVar EvenStreamTable -> Method -> ByteString -> Stream -> IO ()
insertEvenCache var method path strm@Stream{..} = atomically $ do
    refuseIfEvenClosed var
    modifyTVar var $ \st@EvenStreamTable{..} ->
        st
            { evenConc = evenConc + 1
            , evenTable = IntMap.insert streamNumber strm evenTable
            , evenCache = LRUCache.insert (method, path) strm evenCache
            }

deleteEvenCache :: TVar EvenStreamTable -> Method -> ByteString -> IO ()
deleteEvenCache var m path = atomically $ modifyTVar var $ \st@EvenStreamTable{..} ->
    st{evenCache = LRUCache.delete (m, path) evenCache}

lookupEvenCache
    :: TVar EvenStreamTable -> Method -> ByteString -> IO (Maybe Stream)
lookupEvenCache var m path = LRUCache.lookup (m, path) . evenCache <$> readTVarIO var

getEvenStreams :: TVar EvenStreamTable -> IO (IntMap Stream)
getEvenStreams var = evenTable <$> readTVarIO var

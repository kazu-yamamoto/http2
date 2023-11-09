{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.StreamTable (
    OddStreamTable (..),
    emptyOddStreamTable,
    EvenStreamTable (..),
    emptyEvenStreamTable,
    insertOdd,
    deleteOdd,
    lookupOdd,
    insertEven,
    deleteEven,
    lookupEven,
    insertEvenCache,
    deleteEvenCache,
    lookupEvenCache,
) where

import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Network.HTTP.Types (Method)

import Imports
import Network.HTTP2.Arch.Types (Stream (..))

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
      evenCache :: Cache (Method, ByteString) Stream
    }

emptyEvenStreamTable :: Int -> EvenStreamTable
emptyEvenStreamTable lim = EvenStreamTable 0 IntMap.empty $ emptyCache lim

----------------------------------------------------------------

insertOdd :: IORef OddStreamTable -> IntMap.Key -> Stream -> IO ()
insertOdd ref k v = atomicModifyIORef' ref $ \OddStreamTable{..} ->
    let oddConc' = oddConc + 1
        oddTable' = IntMap.insert k v oddTable
     in (OddStreamTable oddConc' oddTable', ())

deleteOdd :: IORef OddStreamTable -> IntMap.Key -> IO ()
deleteOdd ref k = atomicModifyIORef' ref $ \OddStreamTable{..} ->
    let oddConc' = oddConc - 1
        oddTable' = IntMap.delete k oddTable
     in (OddStreamTable oddConc' oddTable', ())

lookupOdd :: IORef OddStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupOdd ref k = IntMap.lookup k . oddTable <$> readIORef ref

----------------------------------------------------------------

insertEven :: IORef EvenStreamTable -> IntMap.Key -> Stream -> IO ()
insertEven ref k v = atomicModifyIORef' ref $ \EvenStreamTable{..} ->
    let evenConc' = evenConc + 1
        evenTable' = IntMap.insert k v evenTable
     in (EvenStreamTable evenConc' evenTable' evenCache, ())

deleteEven :: IORef EvenStreamTable -> IntMap.Key -> IO ()
deleteEven ref k = atomicModifyIORef' ref $ \EvenStreamTable{..} ->
    let evenConc' = evenConc - 1
        evenTable' = IntMap.delete k evenTable
     in (EvenStreamTable evenConc' evenTable' evenCache, ())

lookupEven :: IORef EvenStreamTable -> IntMap.Key -> IO (Maybe Stream)
lookupEven ref k = IntMap.lookup k . evenTable <$> readIORef ref

insertEvenCache
    :: IORef EvenStreamTable -> Method -> ByteString -> Stream -> IO ()
insertEvenCache ref method path strm@Stream{..} = atomicModifyIORef' ref $ \EvenStreamTable{..} ->
    let evenConc' = evenConc + 1
        evenTable' = IntMap.insert streamNumber strm evenTable
        evenCache' = insertCache (method, path) strm evenCache
     in (EvenStreamTable evenConc' evenTable' evenCache', ())

deleteEvenCache :: IORef EvenStreamTable -> Method -> ByteString -> IO ()
deleteEvenCache ref m path = atomicModifyIORef' ref $ \EvenStreamTable{..} ->
    let evenCache' = deleteCache (m, path) evenCache
     in (EvenStreamTable evenConc evenTable evenCache', ())

lookupEvenCache
    :: IORef EvenStreamTable -> Method -> ByteString -> IO (Maybe Stream)
lookupEvenCache ref m path = lookupCache (m, path) . evenCache <$> readIORef ref

----------------------------------------------------------------

type Priority = Int

data Cache k v = Cache
    { cLimit :: Int
    , cSize :: Int
    , cTick :: Priority
    , cQueue :: OrdPSQ k Priority v
    }

emptyCache :: Int -> Cache k v
emptyCache lim = Cache lim 0 0 PSQ.empty

insertCache :: Ord k => k -> v -> Cache k v -> Cache k v
insertCache k v c@Cache{..}
    | cSize == cLimit =
        let q = PSQ.insert k cTick v $ PSQ.deleteMin cQueue
         in c{cTick = cTick + 1, cQueue = q}
    | otherwise =
        let q = PSQ.insert k cTick v cQueue
         in c{cTick = cTick + 1, cQueue = q, cSize = cSize + 1}

lookupCache :: Ord k => k -> Cache k v -> Maybe v
lookupCache k Cache{..} = snd <$> PSQ.lookup k cQueue

deleteCache :: Ord k => k -> Cache k v -> Cache k v
deleteCache k c@Cache{..} =
    let q = PSQ.delete k cQueue
     in c{cQueue = q, cSize = cSize - 1}

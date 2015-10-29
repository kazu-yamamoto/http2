{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BinaryHeapIO (
    Entry
  , newEntry
  , renewEntry
  , item
  , PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  , delete
  ) where

import Control.Monad (when, void)
import Data.Array (Array, listArray, (!))
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64

-- | Abstract data type of entries for priority queues.
data Entry a = Entry {
    item :: a -- ^ Extracting an item from an entry.
  , weight  :: {-# UNPACK #-} !Weight
  , deficit :: {-# UNPACK #-} !Deficit
  } deriving Show

instance Eq (Entry a) where
    Entry _ _ p1 == Entry _ _ p2 = p1 == p2

instance Ord (Entry a) where
    Entry _ _ p1 <  Entry _ _ p2 = p1 <  p2
    Entry _ _ p1 <= Entry _ _ p2 = p1 <= p2

newEntry :: a -> Weight -> Entry a
newEntry x w = Entry x w 0

-- | Changing the item of an entry.
renewEntry :: Entry a -> b -> Entry b
renewEntry ent x = ent { item = x }

----------------------------------------------------------------

type Index = Int
type MA a = IOArray Index (Entry a, Key)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (IORef Deficit)
                                     (IORef Index)
                                     (MA a)
                                     (IORef (IntMap Index))

----------------------------------------------------------------

magicDeficit :: Deficit
magicDeficit = 0

deficitSteps :: Int
deficitSteps = 65536

deficitList :: [Deficit]
deficitList = map calc idxs
  where
    idxs = [1..256] :: [Double]
    calc w = round (fromIntegral deficitSteps / w)

deficitTable :: Array Index Deficit
deficitTable = listArray (1,256) deficitList

weightToDeficit :: Weight -> Deficit
weightToDeficit w = deficitTable ! w

----------------------------------------------------------------

new :: Int -> IO (PriorityQueue a)
new n = PriorityQueue <$> newIORef 0
                      <*> newIORef 1
                      <*> newArray_ (1,n)
                      <*> newIORef I.empty

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Key -> Entry a -> PriorityQueue a -> IO ()
enqueue k Entry{..} (PriorityQueue bref idx arr iref) = do
    i <- readIORef idx
    base <- readIORef bref
    let !b = if deficit == magicDeficit then base else deficit
        !deficit' = b + weightToDeficit weight
        !ent' = Entry item weight deficit'
    writeArray arr i (ent',k)
    modifyIORef' iref (I.insert k i)
    shiftUp arr i iref
    let !i' = i + 1
    writeIORef idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Key, Entry a)
dequeue (PriorityQueue bref idx arr iref) = do
    x@(_,ent) <- shrink arr 1 idx iref
    i <- readIORef idx
    shiftDown arr 1 i iref
    writeIORef bref $ if i == 1 then 0 else deficit ent
    return x

shrink :: MA a -> Index -> IORef Index -> IORef (IntMap Index) -> IO (Key,Entry a)
shrink arr r idx iref = do
    (entr,kr) <- readArray arr r
    -- fixme: checking if i == 0
    i <- subtract 1 <$> readIORef idx
    xi@(_,ki) <- readArray arr i
    writeArray arr r xi
    modifyIORef' iref (I.insert ki r)
    modifyIORef' iref (I.delete kr)
    writeIORef idx i
    return (kr,entr)

shiftUp :: MA a -> Int -> IORef (IntMap Index) -> IO ()
shiftUp _   1 _    = return ()
shiftUp arr c iref = do
    swapped <- swap arr p c iref
    when swapped $ shiftUp arr p iref
  where
    p = c `div` 2

shiftDown :: MA a -> Int -> Int -> IORef (IntMap Index) -> IO ()
shiftDown arr p n iref
  | c1 > n    = return ()
  | c1 == n   = void $ swap arr p c1 iref
  | otherwise = do
      let !c2 = c1 + 1
      xc1 <- readArray arr c1
      xc2 <- readArray arr c2
      let !c = if xc1 < xc2 then c1 else c2
      swapped <- swap arr p c iref
      when swapped $ shiftDown arr c n iref
  where
    c1 = 2 * p

{-# INLINE swap #-}
swap :: MA a -> Int -> Int -> IORef (IntMap Index) -> IO Bool
swap arr p c iref = do
    xp@(_,kp) <- readArray arr p
    xc@(_,kc) <- readArray arr c
    if xc < xp then do
        writeArray arr c xp
        writeArray arr p xc
        modifyIORef' iref (I.insert kp c)
        modifyIORef' iref (I.insert kc p)
        return True
      else
        return False

delete :: Key -> PriorityQueue a -> IO ()
delete k pq@(PriorityQueue _ idx arr iref) = do
    Just r <- I.lookup k <$> readIORef iref
    if r == 1 then
        void $ dequeue pq
      else do
        _ <- shrink arr r idx iref
        i <- readIORef idx
        shiftDown arr r i iref
        shiftUp arr r iref
        return ()

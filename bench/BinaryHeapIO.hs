{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BinaryHeapIO (
    PriorityQueue(..)
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
type Index = Int

----------------------------------------------------------------

data Entry a = Entry {-# UNPACK #-} !Deficit
                     {-# UNPACK #-} !Key
                     {-# UNPACK #-} !Weight
                     a

instance Eq (Entry a) where
    Entry d1 _ _ _ == Entry d2 _ _ _ = d1 == d2

instance Ord (Entry a) where
    Entry d1 _ _ _ <  Entry d2 _ _ _ = d1 <  d2
    Entry d1 _ _ _ <= Entry d2 _ _ _ = d1 <= d2

----------------------------------------------------------------

type MA a = IOArray Index (Entry a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (IORef Deficit)
                                     (IORef Index)
                                     (MA a)
                                     (IORef (IntMap Deficit))

----------------------------------------------------------------

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
enqueue :: Key -> Weight -> a -> PriorityQueue a -> IO ()
enqueue k w x (PriorityQueue bref idx arr dmapref) = do
    i <- readIORef idx
    base <- readIORef bref
    dmap <- readIORef dmapref
    let !b = case I.lookup k dmap of
            Nothing      -> base
            Just deficit -> deficit
        !deficit' = b + weightToDeficit w
        !ent = Entry deficit' k w x
    writeArray arr i ent
    shiftUp arr i
    let !i' = i + 1
    writeIORef idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Key, Weight, a)
dequeue (PriorityQueue bref idx arr dmapref) = do
    Entry d k w x <- shrink arr 1 idx
    i <- readIORef idx
    shiftDown arr 1 i
    if i == 1 then do
        writeIORef bref 0
        writeIORef dmapref I.empty
      else
        writeIORef bref d
    return (k, w, x)

shrink :: MA a -> Index -> IORef Index -> IO (Entry a)
shrink arr r idx = do
    entr <- readArray arr r
    -- fixme: checking if i == 0
    i <- subtract 1 <$> readIORef idx
    xi <- readArray arr i
    writeArray arr r xi
    writeIORef idx i
    return entr

shiftUp :: MA a -> Int -> IO ()
shiftUp _   1 = return ()
shiftUp arr c = do
    swapped <- swap arr p c
    when swapped $ shiftUp arr p
  where
    p = c `div` 2

shiftDown :: MA a -> Int -> Int -> IO ()
shiftDown arr p n
  | c1 > n    = return ()
  | c1 == n   = void $ swap arr p c1
  | otherwise = do
      let !c2 = c1 + 1
      xc1 <- readArray arr c1
      xc2 <- readArray arr c2
      let !c = if xc1 < xc2 then c1 else c2
      swapped <- swap arr p c
      when swapped $ shiftDown arr c n
  where
    c1 = 2 * p

{-# INLINE swap #-}
swap :: MA a -> Int -> Int -> IO Bool
swap arr p c = do
    xp <- readArray arr p
    xc <- readArray arr c
    if xc < xp then do
        writeArray arr c xp
        writeArray arr p xc
        return True
      else
        return False

delete :: Key -> PriorityQueue a -> IO ()
delete k pq@(PriorityQueue _ idx arr dmapref) = do
    i <- readIORef idx
    r <- find k arr i
    if r == 1 then
        void $ dequeue pq
      else do
        _ <- shrink arr r idx
        shiftDown arr r (i - 1)
        shiftUp arr r
    modifyIORef' dmapref (I.delete k)

find :: Key -> MA a -> Index -> IO Index
find k arr lim = go 1
  where
    go !n
      | n == lim  = error "find"
      | otherwise = do
            Entry _ kn _ _ <- readArray arr n
            if kn == k then
                return n
              else
                go (n + 1)

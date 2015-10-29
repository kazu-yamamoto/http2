{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BinaryHeap (
    PriorityQueue(..)
  , new
  , enqueue
  , dequeue
  , delete
  ) where

import Control.Concurrent.STM
import Control.Monad (when, void)
import Data.Array (Array, listArray, (!))
import Data.Array.MArray (newArray_, readArray, writeArray)
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

type MA a = TArray Index (Entry a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (TVar Deficit)
                                     (TVar Index)
                                     (MA a)
                                     (TVar (IntMap Deficit))

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

new :: Int -> STM (PriorityQueue a)
new n = PriorityQueue <$> newTVar 0
                      <*> newTVar 1
                      <*> newArray_ (1,n)
                      <*> newTVar I.empty

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Key -> Weight -> a -> PriorityQueue a -> STM ()
enqueue k w x (PriorityQueue bref idx arr dmapref) = do
    i <- readTVar idx
    base <- readTVar bref
    dmap <- readTVar dmapref
    let !d = weightToDeficit w
        !forNew = base + d
        f _ _ old = old + d
        (!mold, !dmap') = I.insertLookupWithKey f k forNew dmap
        !deficit' = case mold of
            Nothing  -> forNew
            Just old -> old + d
        !ent = Entry deficit' k w x
    writeArray arr i ent
    shiftUp arr i
    let !i' = i + 1
    writeTVar idx i'
    writeTVar dmapref dmap'

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> STM (Maybe (Key, Weight, a))
dequeue (PriorityQueue bref idx arr dmapref) = do
    i <- readTVar idx
    if i == 1 then
        return Nothing
      else do
        Entry d k w x <- shrink arr 1 idx
        j <- readTVar idx
        shiftDown arr 1 j
        if j == 1 then do
            writeTVar bref 0
            writeTVar dmapref I.empty
          else
            writeTVar bref d
        return $ Just (k, w, x)

shrink :: MA a -> Index -> TVar Index -> STM (Entry a)
shrink arr r idx = do
    entr <- readArray arr r
    -- fixme: checking if i == 0
    i <- subtract 1 <$> readTVar idx
    xi <- readArray arr i
    writeArray arr r xi
    writeTVar idx i
    return entr

shiftUp :: MA a -> Int -> STM ()
shiftUp _   1 = return ()
shiftUp arr c = do
    swapped <- swap arr p c
    when swapped $ shiftUp arr p
  where
    p = c `div` 2

shiftDown :: MA a -> Int -> Int -> STM ()
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
swap :: MA a -> Int -> Int -> STM Bool
swap arr p c = do
    xp <- readArray arr p
    xc <- readArray arr c
    if xc < xp then do
        writeArray arr c xp
        writeArray arr p xc
        return True
      else
        return False

delete :: Key -> PriorityQueue a -> STM (Maybe a)
delete k pq@(PriorityQueue _ idx arr dmapref) = do
    i <- readTVar idx
    if i == 1 then
        return Nothing
      else do
        modifyTVar' dmapref (I.delete k)
        r <- find k arr i
        if r == 1 then
            fmap (\(_,_,x) -> x) <$> dequeue pq
          else do
            Entry _ _ _ x <- shrink arr r idx
            shiftDown arr r (i - 1)
            shiftUp arr r
            return $ Just x

find :: Key -> MA a -> Index -> STM Index
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

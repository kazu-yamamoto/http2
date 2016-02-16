{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts, CPP #-}

module BinaryHeapSTM (
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

#if __GLASGOW_HASKELL__ < 709
import Data.Word (Word)
#endif
import Control.Concurrent.STM
import Control.Monad (when, void)
import Data.Array (Array, listArray, (!))
import Data.Array.MArray (newArray_, readArray, writeArray)

----------------------------------------------------------------

type Weight = Int
type Deficit = Word

-- | Abstract data type of entries for priority queues.
data Entry a = Entry {
    weight  :: {-# UNPACK #-} !Weight
  , item    :: {-# UNPACK #-} !(TVar a) -- ^ Extracting an item from an entry.
  , deficit :: {-# UNPACK #-} !(TVar Deficit)
  , index   :: {-# UNPACK #-} !(TVar Index)
  }

newEntry :: a -> Weight -> STM (Entry a)
newEntry x w = Entry w <$> newTVar x <*> newTVar magicDeficit <*> newTVar (-1)

-- | Changing the item of an entry.
renewEntry :: Entry a -> a -> STM ()
renewEntry Entry{..} x = writeTVar item x

----------------------------------------------------------------

type Index = Int
type MA a = TArray Index (Entry a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (TVar Deficit)
                                     (TVar Index)
                                     (MA a)

----------------------------------------------------------------

magicDeficit :: Deficit
magicDeficit = 0

deficitSteps :: Int
deficitSteps = 65536

deficitStepsW :: Word
deficitStepsW = fromIntegral deficitSteps

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

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> STM ()
enqueue ent@Entry{..} (PriorityQueue bref idx arr) = do
    i <- readTVar idx
    base <- readTVar bref
    d <- readTVar deficit
    let !b = if d == magicDeficit then base else d
        !d' = b + weightToDeficit weight
    writeTVar deficit d'
    write arr i ent
    shiftUp arr i
    let !i' = i + 1
    writeTVar idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> STM (Entry a)
dequeue (PriorityQueue bref idx arr) = do
    ent <- shrink arr 1 idx
    i <- readTVar idx
    shiftDown arr 1 i
    d <- readTVar $ deficit ent
    writeTVar bref $ if i == 1 then 0 else d
    return ent

shrink :: MA a -> Index -> TVar Index -> STM (Entry a)
shrink arr r idx = do
    entr <- readArray arr r
    -- fixme: checking if i == 0
    i <- subtract 1 <$> readTVar idx
    xi <- readArray arr i
    write arr r xi
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
      d1 <- readTVar $ deficit xc1
      d2 <- readTVar $ deficit xc2
      let !c = if d1 /= d2 && d2 - d1 <= deficitStepsW then c1 else c2
      swapped <- swap arr p c
      when swapped $ shiftDown arr c n
  where
    c1 = 2 * p

{-# INLINE swap #-}
swap :: MA a -> Index -> Index -> STM Bool
swap arr p c = do
    xp <- readArray arr p
    xc <- readArray arr c
    dp <- readTVar $ deficit xp
    dc <- readTVar $ deficit xc
    if dc < dp then do
        write arr c xp
        write arr p xc
        return True
      else
        return False

{-# INLINE write #-}
write :: MA a -> Index -> Entry a -> STM ()
write arr i ent = do
    writeArray arr i ent
    writeTVar (index ent) i

delete :: Entry a -> PriorityQueue a -> STM ()
delete ent pq@(PriorityQueue _ idx arr) = do
    i <- readTVar $ index ent
    if i == 1 then
        void $ dequeue pq
      else do
        entr <- shrink arr i idx
        r <- readTVar $ index entr
        shiftDown arr r (i - 1)
        shiftUp arr r

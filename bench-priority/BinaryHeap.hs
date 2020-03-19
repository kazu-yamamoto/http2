{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts, CPP #-}

module BinaryHeap (
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
import Control.Monad (when, void)
import Data.Array (Array, listArray, (!))
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.IORef

----------------------------------------------------------------

type Weight = Int
type Deficit = Word

-- | Abstract data type of entries for priority queues.
--   This does not contain Key because Entry is assumed to be stored
--   in HTTP/2 stream information, too.
data Entry a = Entry {
    weight  :: {-# UNPACK #-} !Weight
  , item    :: {-# UNPACK #-} !(IORef a) -- ^ Extracting an item from an entry.
  , deficit :: {-# UNPACK #-} !(IORef Deficit)
  , index   :: {-# UNPACK #-} !(IORef Index)
  }

newEntry :: a -> Weight -> IO (Entry a)
newEntry x w = Entry w <$> newIORef x <*> newIORef magicDeficit <*> newIORef (-1)

-- | Changing the item of an entry.
renewEntry :: Entry a -> a -> IO ()
renewEntry Entry{..} x = writeIORef item x

----------------------------------------------------------------

type Index = Int
type MA a = IOArray Index (Entry a)

data PriorityQueue a = PriorityQueue (IORef Deficit)
                                     (IORef Index)
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

new :: Int -> IO (PriorityQueue a)
new n = PriorityQueue <$> newIORef 0
                      <*> newIORef 1
                      <*> newArray_ (1,n)

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> IO ()
enqueue ent@Entry{..} (PriorityQueue bref idx arr) = do
    i <- readIORef idx
    base <- readIORef bref
    d <- readIORef deficit
    let !b = if d == magicDeficit then base else d
        !d' = b + weightToDeficit weight
    writeIORef deficit d'
    write arr i ent
    shiftUp arr i
    let !i' = i + 1
    writeIORef idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Entry a)
dequeue (PriorityQueue bref idx arr) = do
    ent <- shrink arr 1 idx
    i <- readIORef idx
    shiftDown arr 1 i
    d <- readIORef $ deficit ent
    writeIORef bref $ if i == 1 then 0 else d
    return ent

shrink :: MA a -> Index -> IORef Index -> IO (Entry a)
shrink arr r idx = do
    entr <- readArray arr r
    -- fixme: checking if i == 0
    i <- subtract 1 <$> readIORef idx
    xi <- readArray arr i
    write arr r xi
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
      d1 <- readIORef $ deficit xc1
      d2 <- readIORef $ deficit xc2
      let !c = if d1 /= d2 && d2 - d1 <= deficitStepsW then c1 else c2
      swapped <- swap arr p c
      when swapped $ shiftDown arr c n
  where
    c1 = 2 * p

{-# INLINE swap #-}
swap :: MA a -> Index -> Index -> IO Bool
swap arr p c = do
    xp <- readArray arr p
    xc <- readArray arr c
    dp <- readIORef $ deficit xp
    dc <- readIORef $ deficit xc
    if dc < dp then do
        write arr c xp
        write arr p xc
        return True
      else
        return False

{-# INLINE write #-}
write :: MA a -> Index -> Entry a -> IO ()
write arr i ent = do
    writeArray arr i ent
    writeIORef (index ent) i

delete :: Entry a -> PriorityQueue a -> IO ()
delete ent pq@(PriorityQueue _ idx arr) = do
    i <- readIORef $ index ent
    if i == 1 then
        void $ dequeue pq
      else do
        entr <- shrink arr i idx
        r <- readIORef $ index entr
        shiftDown arr r (i - 1)
        shiftUp arr r

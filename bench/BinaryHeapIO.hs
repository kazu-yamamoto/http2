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
  ) where

import Control.Monad (when, void)
import Data.Array (Array, listArray, (!))
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.IORef
import Data.Word (Word64)

----------------------------------------------------------------

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
type MA a = IOArray Index (Entry a)

-- FIXME: The base (Word64) would be overflowed.
--        In that case, the heap must be re-constructed.
data PriorityQueue a = PriorityQueue (IORef Deficit)
                                     (IORef Index)
                                     (MA a)

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
new n = PriorityQueue <$> newIORef 0 <*> newIORef 1 <*> newArray_ (1,n)

-- | Enqueuing an entry. PriorityQueue is updated.
enqueue :: Entry a -> PriorityQueue a -> IO ()
enqueue Entry{..} (PriorityQueue bvar idx arr) = do
    i <- readIORef idx
    base <- readIORef bvar
    let !b = if deficit == magicDeficit then base else deficit
        !deficit' = b + weightToDeficit weight
        !ent' = Entry item weight deficit'
    writeArray arr i ent'
    shiftUp arr i
    let !i' = i + 1
    writeIORef idx i'
    return ()

-- | Dequeuing an entry. PriorityQueue is updated.
dequeue :: PriorityQueue a -> IO (Entry a)
dequeue (PriorityQueue bvar idx arr) = do
    ent <- readArray arr 1
    i <- readIORef idx
    -- fixme: checking if i == 0
    let i' = i - 1
    readArray arr i' >>= writeArray arr 1
    shiftDown arr 1 i'
    writeIORef idx i'
    writeIORef bvar $ if i' == 1 then 0 else deficit ent
    return ent

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

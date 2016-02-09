{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Priority.PSQ (
    Key
  , Precedence(..)
  , newPrecedence
  , PriorityQueue(..)
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , delete
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.Array (Array, listArray, (!))
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as P
import Data.Word (Word64)

----------------------------------------------------------------

type Key = Int
type Weight = Int
type Deficit = Word64

-- | Internal representation of priority in priority queues.
--   The precedence of a dequeued entry should be specified
--   to 'enqueue' when the entry is enqueued again.
data Precedence = Precedence {
    deficit    :: {-# UNPACK #-} !Deficit
  , weight     :: {-# UNPACK #-} !Weight
  -- stream dependency, used by the upper layer
  , dependency :: {-# UNPACK #-} !Key
  } deriving Show

-- | For test only
newPrecedence :: Weight -> Precedence
newPrecedence w = Precedence 0 w 0

instance Eq Precedence where
  Precedence d1 _ _ == Precedence d2 _ _ = d1 == d2

instance Ord Precedence where
  Precedence d1 _ _ <  Precedence d2 _ _ = d1 <  d2
  Precedence d1 _ _ <= Precedence d2 _ _ = d1 <= d2

type Heap a = IntPSQ Precedence a

data PriorityQueue a = PriorityQueue {
    baseDeficit :: {-# UNPACK #-} !Deficit
  , queue :: !(Heap a)
  }

----------------------------------------------------------------

deficitSteps :: Int
deficitSteps = 65536

deficitList :: [Deficit]
deficitList = map calc idxs
  where
    idxs = [1..256] :: [Double]
    calc w = round (fromIntegral deficitSteps / w)

deficitTable :: Array Int Deficit
deficitTable = listArray (1,256) deficitList

weightToDeficit :: Weight -> Deficit
weightToDeficit w = deficitTable ! w

deficitLimit :: Deficit
deficitLimit = 10000000000000000000 -- more than 2^63 and less than 2^63 + 2^62

----------------------------------------------------------------

empty :: PriorityQueue a
empty = PriorityQueue 0 P.empty

isEmpty :: PriorityQueue a -> Bool
isEmpty PriorityQueue{..} = P.null queue

enqueue :: Key -> Precedence -> a -> PriorityQueue a -> PriorityQueue a
enqueue k p v PriorityQueue{..}
  | deficit' < deficitLimit = fastPath
  | otherwise               = slowPath
  where
    !d = weightToDeficit (weight p)
    !b = if deficit p == 0 then baseDeficit else deficit p
    !deficit' = b + d
    fastPath = PriorityQueue baseDeficit queue'
      where
        !p' = p { deficit = deficit' }
        !queue' = P.insert k p' v queue
    slowPath = PriorityQueue 0 queue''
      where
        adjust (x,y,z) = (x,y',z)
          where
            !d' = deficit y - baseDeficit
            !y' = y { deficit = d' }
        !queue' = P.fromList $ map adjust $ P.toList queue
        !deficit'' = deficit' - baseDeficit
        !p'' = p { deficit = deficit'' }
        !queue'' = P.insert k p'' v queue'

dequeue :: PriorityQueue a -> Maybe (Key, Precedence, a, PriorityQueue a)
dequeue PriorityQueue{..} = case P.minView queue of
    Nothing                -> Nothing
    Just (k, p, v, queue') -> let !base = deficit p
                              in Just (k, p, v, PriorityQueue base queue')

delete :: Key -> PriorityQueue a -> (Maybe a, PriorityQueue a)
delete k q@PriorityQueue{..} = case P.alter f k queue of
    (mv@(Just _), queue') -> case P.minView queue of
        Nothing          -> error "delete"
        Just (k',p',_,_)
          | k' == k      -> (mv, PriorityQueue (deficit p') queue')
          | otherwise    -> (mv, PriorityQueue baseDeficit queue')
    (Nothing, _)         -> (Nothing, q)
  where
    f Nothing      = (Nothing, Nothing)
    f (Just (_,v)) = (Just v,  Nothing)

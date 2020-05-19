{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Cache where

import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

type Priority = Int

data Cache k v = Cache {
    cLimit :: Int
  , cSize  :: Int
  , cTick  :: Priority
  , cQueue :: OrdPSQ k Priority v
  }

emptyCache :: Int -> Cache k v
emptyCache lim = Cache lim 0 0 PSQ.empty

insert :: Ord k => k -> v -> Cache k v -> Cache k v
insert k v c@Cache{..}
  | cSize == cLimit = let q = PSQ.insert k cTick v $ PSQ.deleteMin cQueue
                      in c { cTick = cTick + 1, cQueue = q }
  | otherwise       = let q = PSQ.insert k cTick v cQueue
                      in c { cTick = cTick + 1, cQueue = q, cSize = cSize + 1 }

lookup :: Ord k => k -> Cache k v -> Maybe v
lookup k Cache{..} = snd <$> PSQ.lookup k cQueue

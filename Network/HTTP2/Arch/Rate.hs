module Network.HTTP2.Arch.Rate (
    Rate
  , newRate
  , getRate
  ) where

import Data.IORef
import Data.UnixTime

newtype Rate = Rate (IORef Counter)

data Counter = Counter Int UnixTime

newRate :: IO Rate
newRate = do
    cntr <- Counter 0 <$> getUnixTime
    Rate <$> newIORef cntr

getRate :: Rate -> IO Int
getRate (Rate ref) = do
    Counter n beg <- readIORef ref
    cur <- getUnixTime
    if (cur `diffUnixTime` beg) > 1 then do
        let n' = 1
        writeIORef ref $ Counter n' cur
        return n'
      else do
        let n' = n + 1
        writeIORef ref $ Counter n' beg
        return n'

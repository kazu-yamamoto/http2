module Network.HTTP2.Server.ReadN where

import qualified Data.ByteString as B
import Data.IORef
import Network.Socket
import Network.Socket.ByteString (recv)

-- | Naive implementation for readN.
defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> Int -> IO B.ByteString
defaultReadN s ref n = do
    mbs <- readIORef ref
    writeIORef ref Nothing
    case mbs of
      Nothing -> do
          bs <- recv s n
          if B.length bs == n then
              return bs
            else
              loop bs
      Just bs
        | B.length bs == n -> return bs
        | B.length bs > n  -> do
              let (bs0, bs1) = B.splitAt n bs
              writeIORef ref (Just bs1)
              return bs0
        | otherwise        -> loop bs
  where
    loop bs = do
        let n' = n - B.length bs
        bs1 <- recv s n'
        let bs2 = bs `B.append` bs1
        if B.length bs2 == n then
            return bs2
          else
            loop bs2

module Network.HTTP2.Arch.ReadN where

import qualified Data.ByteString as B
import Data.IORef
import Network.Socket
import qualified Network.Socket.ByteString as N

-- | Naive implementation for readN.
defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> Int -> IO B.ByteString
defaultReadN _ _ 0 = return B.empty
defaultReadN s ref n = do
    mbs <- readIORef ref
    writeIORef ref Nothing
    case mbs of
        Nothing -> do
            bs <- N.recv s n
            if B.null bs
                then return B.empty
                else
                    if B.length bs == n
                        then return bs
                        else loop bs
        Just bs
            | B.length bs == n -> return bs
            | B.length bs > n -> do
                let (bs0, bs1) = B.splitAt n bs
                writeIORef ref (Just bs1)
                return bs0
            | otherwise -> loop bs
  where
    loop bs = do
        let n' = n - B.length bs
        bs1 <- N.recv s n'
        if B.null bs1
            then return B.empty
            else do
                let bs2 = bs `B.append` bs1
                if B.length bs2 == n
                    then return bs2
                    else loop bs2

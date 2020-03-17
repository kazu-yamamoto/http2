module Network.HTTP2.Server.File where

import System.IO

import Network.HTTP2.Types

-- | Position read based on 'Handle'.
defaultPositionReadMaker :: PositionReadMaker
defaultPositionReadMaker file = do
    hdl <- openBinaryFile file ReadMode
    return (pread hdl, Closer $ hClose hdl)
  where
    pread :: Handle -> PositionRead
    pread hdl off bytes buf = do
        hSeek hdl AbsoluteSeek $ fromIntegral off
        fromIntegral <$> (hGetBufSome hdl buf $ fromIntegral bytes)

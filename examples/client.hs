module Main where

import Control.Concurrent
import Data.ByteString.Char8
import Network.HTTP2.Client
import Network.Run.TCP
import qualified Network.Socket.ByteString as NSB

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" $ \sock -> do
    let conf = Config (NSB.sendAll sock) (NSB.recv sock)
    run conf $ \client -> do
        _ <- forkIO $ client defaultRequest{ requestAuthority = pack authority } print
        client defaultRequest{ requestAuthority = pack authority } print

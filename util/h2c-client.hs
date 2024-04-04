module Main where

import qualified Control.Exception as E
import Network.HTTP2.Client
import Network.Run.TCP (runTCPClient)
import System.Environment
import System.Exit

import Client

main :: IO ()
main = do
    args <- getArgs
    (host, port) <- case args of
        [h, p] -> return (h, p)
        _ -> do
            putStrLn "client <addr> <port>"
            exitFailure
    runTCPClient host port $ runHTTP2Client host
  where
    cliconf host = defaultClientConfig{authority = host}
    runHTTP2Client host s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run (cliconf host) conf client)

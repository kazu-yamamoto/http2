{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import Network.HTTP2.Server
import Network.Run.TCP
import System.Environment
import System.Exit

import Server

main :: IO ()
main = do
    args <- getArgs
    (host, port) <- case args of
        [h, p] -> return (h, p)
        _ -> do
            putStrLn "server <addr> <port>"
            exitFailure
    runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf server)

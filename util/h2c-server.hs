{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.HTTP2.Server
import Network.Run.TCP
import System.Console.GetOpt
import System.Environment
import System.Exit
import qualified UnliftIO.Exception as E

import Server

options :: [OptDescr (Options -> Options)]
options = []

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

serverOpts :: [String] -> IO (Options, [String])
serverOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

data Options = Options deriving (Show)

defaultOptions :: Options
defaultOptions = Options

usage :: String
usage = "Usage: h2c-server [OPTION] <addr> <port>"

main :: IO ()
main = do
    args <- getArgs
    (Options, ips) <- serverOpts args
    (host, port) <- case ips of
        [h, p] -> return (h, p)
        _ -> showUsageAndExit usage
    runTCPServer (Just host) port $ \s ->
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf server)

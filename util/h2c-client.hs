{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Network.HTTP2.Client
import Network.Run.TCP (runTCPClient)
import System.Console.GetOpt
import System.Environment
import System.Exit

import Client
import Monitor

defaultOptions :: Options
defaultOptions =
    Options
        { optPerformance = 0
        , optNumOfReqs = 1
        , optMonitor = False
        , optInteractive = False
        }

usage :: String
usage = "Usage: h2c-client [OPTION] addr port [path]"

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['t']
        ["performance"]
        (ReqArg (\n o -> o{optPerformance = read n}) "<size>")
        "measure performance"
    , Option
        ['n']
        ["number-of-requests"]
        (ReqArg (\n o -> o{optNumOfReqs = read n}) "<n>")
        "specify the number of requests"
    , Option
        ['m']
        ["monitor"]
        (NoArg (\opts -> opts{optMonitor = True}))
        "run thread monitor"
    , Option
        ['i']
        ["interactive"]
        (NoArg (\o -> o{optInteractive = True}))
        "enter interactive mode"
    ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

clientOpts :: [String] -> IO (Options, [String])
clientOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

main :: IO ()
main = do
    labelMe "h2c-client main"
    args <- getArgs
    (opts, ips) <- clientOpts args
    (host, port, paths) <- case ips of
        [] -> showUsageAndExit usage
        _ : [] -> showUsageAndExit usage
        h : p : [] -> return (h, p, ["/"])
        h : p : ps -> return (h, p, C8.pack <$> ps)
    when (optMonitor opts) $ void $ forkIO $ monitor $ threadDelay 1000000
    let cliconf = defaultClientConfig{authority = host}
    launch cliconf opts host port paths

launch :: ClientConfig -> Options -> String -> String -> [Path] -> IO ()
launch cliconf opts host port paths = do
    runTCPClient host port $ \s ->
        E.bracket
            (allocSimpleConfig' s 4096 5000000)
            freeSimpleConfig
            doit
  where
    doit conf = run cliconf conf go
    go :: Client ()
    go sendRequest _aux
        | optInteractive opts = do
            let action = client opts paths sendRequest _aux
            console opts paths action _aux
            return ()
        | otherwise = client opts paths sendRequest _aux

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as C8
import Network.HTTP2.Client
import Network.Run.TCP (runTCPClient)
import System.Console.GetOpt
import System.Environment
import System.Exit
import qualified UnliftIO.Exception as E

import Client

data Options = Options
    { optNumOfReqs :: Int
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optNumOfReqs = 1
        }

usage :: String
usage = "Usage: h2c-client [OPTION] addr port [path]"

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['n']
        ["number-of-requests"]
        (ReqArg (\n o -> o{optNumOfReqs = read n}) "<n>")
        "specify the number of requests"
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
    args <- getArgs
    (Options{..}, ips) <- clientOpts args
    (host, port, paths) <- case ips of
        [] -> showUsageAndExit usage
        _ : [] -> showUsageAndExit usage
        h : p : [] -> return (h, p, ["/"])
        h : p : ps -> return (h, p, C8.pack <$> ps)
    let cliconf = defaultClientConfig{authority = host}
    runTCPClient host port $ \s ->
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> run cliconf conf $ client optNumOfReqs paths)

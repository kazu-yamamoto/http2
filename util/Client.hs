{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Client where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.UnixTime
import Foreign.C.Types
import Network.HTTP.Types
import System.IO
import Text.Printf

import Network.HTTP2.Client

import Monitor

data Options = Options
    { optPerformance :: Int
    , optNumOfReqs :: Int
    , optMonitor :: Bool
    , optInteractive :: Bool
    }
    deriving (Show)

client :: Options -> [Path] -> Client ()
client Options{..} paths sendRequest _aux = do
    labelMe "h2c client"
    let cli
            | optPerformance /= 0 = clientPF optPerformance sendRequest
            | otherwise = clientNReqs optNumOfReqs sendRequest
    ex <- E.try $ mapConcurrently_ cli paths
    case ex of
        Right () -> return ()
        Left e -> print (e :: HTTP2Error)

clientNReqs :: Int -> SendRequest -> Path -> IO ()
clientNReqs n0 sendRequest path = do
    labelMe "h2c clinet N requests"
    loop n0
  where
    req = requestNoBody methodGet path []
    loop 0 = return ()
    loop n = do
        sendRequest req $ \rsp -> do
            print $ responseStatus rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
        loop (n - 1)

-- Path is dummy
clientPF :: Int -> SendRequest -> Path -> IO ()
clientPF n sendRequest _ = do
    labelMe "h2c clinet performance"
    t1 <- getUnixTime
    sendRequest req loop
    t2 <- getUnixTime
    printThroughput t1 t2 n
  where
    req = requestNoBody methodGet path []
    path = "/perf/" <> C8.pack (show n)
    loop rsp = do
        bs <- getResponseBodyChunk rsp
        when (bs /= "") $ loop rsp

printThroughput :: UnixTime -> UnixTime -> Int -> IO ()
printThroughput t1 t2 n =
    printf
        "Throughput %.2f Mbps (%d bytes in %d msecs)\n"
        bytesPerSeconds
        n
        millisecs
  where
    UnixDiffTime (CTime s) u = t2 `diffUnixTime` t1
    millisecs :: Int
    millisecs = fromIntegral s * 1000 + fromIntegral u `div` 1000
    bytesPerSeconds :: Double
    bytesPerSeconds =
        fromIntegral n
            * (1000 :: Double)
            * 8
            / fromIntegral millisecs
            / 1024
            / 1024

console
    :: Options -> [ByteString] -> IO () -> Aux -> IO ()
console _opt paths cli aux = do
    putStrLn "q -- quit"
    putStrLn "g -- get"
    putStrLn "p -- ping"
    mvar <- newEmptyMVar
    loop mvar `E.catch` \(E.SomeException _) -> return ()
  where
    loop mvar = do
        hSetBuffering stdout NoBuffering
        putStr "> "
        hSetBuffering stdout LineBuffering
        l <- getLine
        case l of
            "q" -> putStrLn "bye"
            "g" -> do
                mapM_ (\p -> putStrLn $ "GET " ++ C8.unpack p) paths
                _ <- forkIO $ cli >> putMVar mvar ()
                takeMVar mvar
                loop mvar
            "p" -> do
                putStrLn "Ping"
                auxSendPing aux
                loop mvar
            _ -> do
                putStrLn "No such command"
                loop mvar

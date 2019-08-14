{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP2.Client
import Network.Run.TCP
import Data.ByteString.Char8

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" $ \sock -> withConnection sock $ \conn -> do
    withResponse conn defaultRequest{ requestAuthority = pack authority } print
    withResponse conn defaultRequest{ requestAuthority = pack authority } print

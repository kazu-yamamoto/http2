{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP2.Client
import Network.Run.TCP
import Data.ByteString.Char8
import qualified Network.Socket.ByteString as NSB

authority :: String
authority = "127.0.0.1"

main :: IO ()
main = runTCPClient authority "80" $ \sock -> do
    let conf = Config (NSB.sendAll sock) (NSB.recv sock)
    withConnection conf $ \conn -> do
        withResponse conn defaultRequest{ requestAuthority = pack authority } print
        withResponse conn defaultRequest{ requestAuthority = pack authority } print

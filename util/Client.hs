{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Client where

import Control.Concurrent.Async
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types

import Network.HTTP2.Client

client :: Client ()
client sendRequest _aux = do
    let req0 = requestNoBody methodGet "/" []
        client0 = sendRequest req0 $ \rsp -> do
            print $ responseStatus rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
        req1 = requestNoBody methodGet "/notfound" []
        client1 = sendRequest req1 $ \rsp -> do
            print $ responseStatus rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
    ex <- E.try $ concurrently_ client0 client1
    case ex of
        Right () -> return ()
        Left e -> print (e :: HTTP2Error)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP2.ClientSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (for_)
import Data.Traversable (for)
import Network.HTTP.Types
import Network.Run.TCP
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Timeout (timeout)
import Test.Hspec

import Network.HTTP2.Client
import qualified Network.HTTP2.Server as S

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (43124 :: Int, 44320)

host :: String
host = "127.0.0.1"

host' :: ByteString
host' = C8.pack host

spec :: Spec
spec = do
    describe "client" $ do
        it "receives an error if scheme is missing" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "" host' (defaultClient []) `shouldThrow` connectionError

        it "receives an error if authority is missing" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" "" (defaultClient []) `shouldThrow` connectionError

        it "receives an error if authority and host are different" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" host' (defaultClient [("Host", "foo")])
                    `shouldThrow` connectionError

        it "does not deadlock (in concurrent setting)" $
            E.bracket (forkIO $ runServer irresponsiveServer) killThread $ \_ -> do
                threadDelay 10000
                resultVar <- newEmptyMVar
                runClient "http" "localhost" $ concurrentClient resultVar
                result <- timeout 1000000 $ takeMVar resultVar
                case result of
                    Nothing -> expectationFailure "Exception was not raised"
                    Just (Left ConnectionIsClosed) -> return ()
                    Just (Left err) -> expectationFailure $ "Raise unexpected exception " ++ show err
                    Just (Right ()) -> expectationFailure "Unexpected client termination"

        it "respects max concurrent streams setting" $
            E.bracket (forkIO $ runServer irresponsiveServer) killThread $ \_ -> do
                threadDelay 10000
                let maxConcurrentStreams = 64

                resultVars <- runClient "http" "localhost" $ \sendReq aux -> do
                    for [1 .. (maxConcurrentStreams + 1) :: Int] $ \_ -> do
                        resultVar <- newEmptyMVar
                        concurrentClient resultVar sendReq aux
                        pure resultVar

                let acceptedRequestVars = take maxConcurrentStreams resultVars
                acceptedResults <- for acceptedRequestVars (timeout 1000000 . takeMVar)
                for_ acceptedResults $ \case
                    Nothing -> expectationFailure "Exception was not raised"
                    Just (Left ConnectionIsClosed) -> return ()
                    Just (Left err) -> expectationFailure $ "Raise unexpected exception " ++ show err
                    Just (Right ()) -> expectationFailure "Unexpected client termination"

                let waitingRequestVars = drop maxConcurrentStreams resultVars
                waitingResults <- for waitingRequestVars (timeout 1000000 . takeMVar)
                for_ waitingResults $ \case
                    Nothing -> pure ()
                    Just (Left err) -> expectationFailure $ "Raise unexpected exception " ++ show err
                    Just (Right ()) -> expectationFailure "Unexpected client termination"

runServer :: S.Server -> IO ()
runServer server = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> S.run S.defaultServerConfig conf server)

defaultServer :: S.Server
defaultServer _req _aux sendResponse = sendResponse responseHello []

irresponsiveServer :: S.Server
irresponsiveServer _req _aux _sendResponse = do
    forever $ threadDelay 10000000

responseHello :: S.Response
responseHello = S.responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

runClient :: Scheme -> Authority -> Client a -> IO a
runClient sc au client = runTCPClient host port $ runHTTP2Client
  where
    cliconf = defaultClientConfig{scheme = sc, authority = au}
    runHTTP2Client s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            ( \conf -> run cliconf conf $ \sendRequest ->
                client sendRequest
            )

defaultClient :: RequestHeaders -> Client ()
defaultClient hd sendRequest _aux = do
    let req = requestNoBody methodGet "/" hd
    sendRequest req $ \rsp -> do
        responseStatus rsp `shouldBe` Just ok200
        fmap statusMessage (responseStatus rsp) `shouldBe` Just "OK"

concurrentClient :: MVar (Either HTTP2Error ()) -> Client ()
concurrentClient resultVar sendRequest _aux = do
    let req = requestNoBody methodGet "/" []
    void $ forkIO $ do
        result <- E.try $ sendRequest req $ \_rsp -> return ()
        putMVar resultVar result
    threadDelay 10000

connectionError :: Selector HTTP2Error
connectionError ConnectionErrorIsReceived{} = True
connectionError _ = False

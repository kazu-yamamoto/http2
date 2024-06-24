{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP2.ClientSpec (spec) where

import Control.Concurrent
import Control.Monad
import Data.ByteString.Builder (byteString)
import Data.Foldable (for_)
import Data.Maybe
import Data.Traversable (for)
import Network.HTTP.Semantics
import Network.HTTP.Types
import Network.Run.TCP
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Timeout (timeout)
import Test.Hspec
import qualified UnliftIO.Exception as E

import Network.HTTP2.Client
import qualified Network.HTTP2.Server as S

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (43124 :: Int, 44320)

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "client" $ do
        it "receives an error if scheme is missing" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "" host (defaultClient []) `shouldThrow` streamError

        it "receives an error if authority is missing" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" "" (defaultClient []) `shouldThrow` streamError

        it "receives an error if authority and host are different" $
            E.bracket (forkIO $ runServer defaultServer) killThread $ \_ -> do
                threadDelay 10000
                runClient "http" host (defaultClient [("Host", "foo")])
                    `shouldThrow` streamError

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
                let maxConc = fromJust $ maxConcurrentStreams defaultSettings

                resultVars <- runClient "http" "localhost" $ \sendReq aux -> do
                    for [1 .. (maxConc + 1) :: Int] $ \_ -> do
                        resultVar <- newEmptyMVar
                        concurrentClient resultVar sendReq aux
                        pure resultVar

                let acceptedRequestVars = take maxConc resultVars
                acceptedResults <- for acceptedRequestVars (timeout 1000000 . takeMVar)
                for_ acceptedResults $ \case
                    Nothing -> expectationFailure "Exception was not raised"
                    Just (Left ConnectionIsClosed) -> return ()
                    Just (Left err) -> expectationFailure $ "Raise unexpected exception " ++ show err
                    Just (Right ()) -> expectationFailure "Unexpected client termination"

                let waitingRequestVars = drop maxConc resultVars
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

streamError :: Selector HTTP2Error
streamError StreamErrorIsReceived{} = True
streamError ConnectionErrorIsReceived{} = True
streamError _ = False

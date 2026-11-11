{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP2.ClientSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString.Builder (byteString)
import Data.Foldable (for_)
import Data.Maybe
import Data.Traversable (for)
import Network.HTTP.Semantics
import Network.HTTP.Types
import Network.Run.TCP hiding (defaultSettings)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
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

spec :: Spec
spec = do
    describe "client" $ do
        it "receives an error if scheme is missing" $
            withServer (runServer defaultServer) $ do
                threadDelay 10000
                runClient "" host (defaultClient []) `shouldThrow` streamError

        it "receives an error if authority is missing" $
            withServer (runServer defaultServer) $ do
                threadDelay 10000
                runClient "http" "" (defaultClient []) `shouldThrow` streamError

        it "receives an error if authority and host are different" $
            withServer (runServer defaultServer) $ do
                threadDelay 10000
                runClient "http" host (defaultClient [("Host", "foo")])
                    `shouldThrow` streamError

        it "does not deadlock (in concurrent setting)" $
            withServer (runServer irresponsiveServer) $ do
                threadDelay 10000
                resultVar <- newEmptyMVar
                runClient "http" "localhost" $ concurrentClient resultVar
                result <- timeout 1000000 $ takeMVar resultVar
                case result of
                    Nothing -> expectationFailure "Exception was not raised"
                    Just (Left ConnectionIsClosed) -> return ()
                    Just (Left err) -> expectationFailure $ "Raise unexpected exception " ++ show err
                    Just (Right ()) -> expectationFailure "Unexpected client termination"

        it "reports a connection the server resets as ConnectionIsReset" $
            withResettingServer $ \p -> do
                r <- E.try $ runClientTo p (defaultClient [])
                case r of
                    Left ConnectionIsReset -> return ()
                    _ -> expectationFailure $ "Unexpected result: " ++ show r

        it "respects max concurrent streams setting" $
            withServer (runServer irresponsiveServer) $ do
                threadDelay 10000
                let maxConc = fromJust $ maxConcurrentStreams defaultSettings

                resultVars <- runClient "http" "localhost" $ \sendReq aux -> do
                    replicateM ((maxConc + 1) :: Int) $ do
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

runServer :: S.Server -> ServerSettings -> IO ()
runServer server set = runTCPServerWithSettings set (Just host) port runHTTP2Server
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
runClient = runClientWith port

runClientTo :: String -> Client a -> IO a
runClientTo p = runClientWith p "http" "localhost"

runClientWith :: String -> Scheme -> Authority -> Client a -> IO a
runClientWith p sc au client = runTCPClient host p runHTTP2Client
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

-- | A malformed request is a stream error (RFC 9113 section 8.1.1), so the
-- server resets that stream and the connection carries on.  The client learns
-- of it through the stream it was waiting on, as 'StreamResetIsReceived'.
--
-- This used to also admit 'ConnectionErrorIsReceived', from back when the
-- server escalated every stream error to the connection and answered one bad
-- request by hanging up on all of them.
streamError :: Selector HTTP2Error
streamError StreamResetIsReceived{} = True
streamError _ = False

-- | A server which answers the client's SETTINGS and then resets the
--   connection, as one killed in the middle of a request does.
withResettingServer :: (String -> IO a) -> IO a
withResettingServer action =
    E.bracket listenLocal NS.close $ \ls -> do
        p <- NS.socketPort ls
        _ <- forkIO $ do
            (s, _) <- NS.accept ls
            _ <- NSB.recv s 4096
            NSB.sendAll s "\0\0\0\4\0\0\0\0\0" -- an empty SETTINGS
            threadDelay 100000
            NS.setSockOpt s NS.Linger (NS.StructLinger 1 0)
            NS.close s
        action $ show p
  where
    listenLocal = do
        ls <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        NS.bind ls $ NS.SockAddrInet 0 $ NS.tupleToHostAddress (127, 0, 0, 1)
        NS.listen ls 1
        return ls

-- | Running a test server for the duration of the action, then telling it
--   to stop.  'killThread' is no good here: it does not reach a thread
--   blocked in @accept@ on Windows, where the call is a safe foreign one,
--   so the whole suite used to hang on the first of these.
withServer :: (ServerSettings -> IO ()) -> IO a -> IO a
withServer runIt action = do
    stopVar <- newEmptyMVar
    done <- newEmptyMVar
    let set = defaultServerSettings{settingsInstallShutdownHandler = putMVar stopVar}
    _ <- forkIO $ runIt set `E.finally` putMVar done ()
    action `E.finally` do
        join $ takeMVar stopVar
        takeMVar done

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP2.WatchdogSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString
import System.IO.Unsafe
import System.Random
import System.Timeout (timeout)
import Test.Hspec

import qualified Network.HTTP2.Client as C
import Network.HTTP2.Frame (connectionPreface)
import Network.HTTP2.Server

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (44321 :: Int, 45320)

host :: String
host = "127.0.0.1"

-- | The timeout of the server, in microseconds.
serverTimeout :: Int
serverTimeout = 1000000

spec :: Spec
spec = describe "watchdog" $ do
    it "closes a connection that stops half way through the preface" $
        -- The preface is read before anything else is set up, so the
        -- watchdog has to start before it rather than after.
        withServer (\_ _ _ -> return ()) $ do
            r <- timeout 3000000 $ runTCPClient host port $ \s -> do
                sendAll s $ B.take 8 connectionPreface
                recvAll s
            -- Closed, with nothing sent: there was no connection to say
            -- GOAWAY on yet.
            r `shouldBe` Just ""

    it "closes an idle connection with GOAWAY" $
        withServer (\_ _ _ -> return ()) $ do
            r <- timeout 3000000 $ runTCPClient host port $ \s -> do
                sendAll s connectionPreface
                sendAll s emptySettingsFrame
                recvAll s
            -- The sender finished by itself and the connection was closed
            -- as usual, with GOAWAY whose debug data is "timeout".
            fmap ("timeout" `B.isSuffixOf`) r `shouldBe` Just True

    it "does not limit a slow application" $
        withServer slowServer $ do
            r <- timeout 5000000 $ get "/"
            r `shouldBe` Just (Just ok200, "slow")

    it "does not limit a streaming application between chunks" $
        withServer streamServer $ do
            r <- timeout 5000000 $ get "/"
            r `shouldBe` Just (Just ok200, "first second")

    it "times out a stalled request body" $ do
        result <- newEmptyMVar
        let server req _aux sendResponse = do
                r <- E.try $ consume $ getRequestBodyChunk req
                putMVar result $
                    either (const False) (const True) (r :: Either E.SomeException ByteString)
                sendResponse (responseNoBody ok200 []) []
        withServer server $ do
            _ <- forkIO $ E.handle ignore $ runTCPClient host port $ \s ->
                E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                    C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                        let req = C.requestStreaming methodPost "/" [] $ \write flush -> do
                                write (byteString "abc") >> flush
                                threadDelay 10000000
                        sendRequest req $ \_ -> return ()
            r <- timeout 3000000 $ takeMVar result
            r `shouldBe` Just False
  where
    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

----------------------------------------------------------------

withServer :: Server -> IO () -> IO ()
withServer server body =
    E.bracket (forkIO runServer) killThread $ \_ -> do
        threadDelay 10000
        body
  where
    runServer = runTCPServer (Just host) port $ \s ->
        E.bracket
            (allocSimpleConfig' s 32768 serverTimeout)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf server `E.catch` ignore)
    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

slowServer :: Server
slowServer _req _aux sendResponse = do
    threadDelay 1500000
    sendResponse (responseBuilder ok200 [] "slow") []

streamServer :: Server
streamServer _req _aux sendResponse =
    sendResponse rsp []
  where
    rsp = responseStreaming ok200 [] $ \write flush -> do
        write (byteString "first ") >> flush
        threadDelay 1500000
        write (byteString "second") >> flush

get :: ByteString -> IO (Maybe Status, ByteString)
get path = runTCPClient host port $ \s ->
    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ ->
            sendRequest (C.requestNoBody methodGet path []) $ \rsp -> do
                body <- consume $ C.getResponseBodyChunk rsp
                return (C.responseStatus rsp, body)

consume :: IO ByteString -> IO ByteString
consume rbody = B.concat <$> loop
  where
    loop = do
        bs <- rbody
        if B.null bs then return [] else (bs :) <$> loop

-- | Receiving until EOF.
recvAll :: Socket -> IO ByteString
recvAll s = B.concat <$> loop
  where
    -- A peer which resets rather than closes is an EOF for our purposes.
    -- Only an 'E.IOException': the caller wraps this in 'timeout', which
    -- ends it by throwing, and catching that would turn "the server never
    -- closed the connection" into bytes it never sent.
    loop = do
        bs <- recv s 4096 `E.catch` \(_ :: E.IOException) -> return ""
        if B.null bs then return [] else (bs :) <$> loop

-- | A SETTINGS frame with no parameters.
emptySettingsFrame :: ByteString
emptySettingsFrame = B.pack [0, 0, 0, 4, 0, 0, 0, 0, 0]

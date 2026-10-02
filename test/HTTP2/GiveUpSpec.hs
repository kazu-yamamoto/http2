{-# LANGUAGE OverloadedStrings #-}

module HTTP2.GiveUpSpec (spec) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.Word (Word8)
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket (
    ShutdownCmd (..),
    SocketOption (..),
    setSocketOption,
    shutdown,
 )
import Network.Socket.ByteString
import System.IO.Unsafe
import System.Random
import System.Timeout (timeout)
import Test.Hspec

import Network.HTTP2.Client (HTTP2Error (..))
import Network.HTTP2.Frame (connectionPreface)
import Network.HTTP2.Server

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (45321 :: Int, 46320)

host :: String
host = "127.0.0.1"

-- | The timeout of the server, in microseconds.
serverTimeout :: Int
serverTimeout = 1000000

spec :: Spec
spec = describe "give-up" $ do
    it "gives up on a write the peer does not read" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse =
                sendResponse (responseStreaming ok200 [] endless) []
            endless write flush = forever $ write (byteString chunk) >> flush
            chunk = B.replicate 65536 0x41
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 serverTimeout) freeSimpleConfig $ \conf ->
                    (run defaultServerConfig conf server `E.catch` ignore)
                        `E.finally` putMVar ended ()
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            -- The client opens the windows wide, asks, and never reads,
            -- so the server's writes stop in the socket, not on a window.
            r <- runTCPClient host port $ \s -> do
                -- A fixed small buffer: one that grows would take the data in.
                setSocketOption s RecvBuffer 4096
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 $ B.pack [0, 4, 0x7f, 0xff, 0xff, 0xff]
                sendAll s $ frame 8 0 0 $ B.pack [0x7f, 0xff, 0, 0]
                sendAll s $ frame 1 5 1 getHeaderBlock
                timeout 6000000 $ takeMVar ended
            r `shouldBe` Just ()
    it "gives up on a write the peer does not read after the receiver ends" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse =
                sendResponse (responseStreaming ok200 [] endless) []
            endless write flush = forever $ write (byteString chunk) >> flush
            chunk = B.replicate 65536 0x41
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 serverTimeout) freeSimpleConfig $ \conf ->
                    (run defaultServerConfig conf server `E.catch` ignore)
                        `E.finally` putMVar ended ()
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            -- As above, but the client ends its side once it has asked, so
            -- the receiver ends on EOF rather than by its timeout.
            r <- runTCPClient host port $ \s -> do
                setSocketOption s RecvBuffer 4096
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 $ B.pack [0, 4, 0x7f, 0xff, 0xff, 0xff]
                sendAll s $ frame 8 0 0 $ B.pack [0x7f, 0xff, 0, 0]
                sendAll s $ frame 1 5 1 getHeaderBlock
                threadDelay 100000
                shutdown s ShutdownSend
                timeout 6000000 $ takeMVar ended
            r `shouldBe` Just ()
    it "gives up on a stream whose window the peer leaves shut" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse =
                sendResponse (responseStreaming ok200 [] endless) []
            endless write flush = forever $ write (byteString chunk) >> flush
            chunk = B.replicate 1000 0x41
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 serverTimeout) freeSimpleConfig $ \conf ->
                    (run defaultServerConfig conf server `E.catch` ignore)
                        `E.finally` putMVar ended ()
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            -- The client asks and then goes silent: the response fills the
            -- windows and the application blocks, with nothing left to write
            -- (#158).
            r <- runTCPClient host port $ \s -> do
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 ""
                sendAll s $ frame 1 5 1 getHeaderBlock
                timeout 6000000 $ takeMVar ended
            r `shouldBe` Just ()
    it "gives up on a stream when the peer leaves the connection window shut" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse =
                sendResponse (responseStreaming ok200 [] endless) []
            endless write flush = forever $ write (byteString chunk) >> flush
            chunk = B.replicate 1000 0x41
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 serverTimeout) freeSimpleConfig $ \conf ->
                    (run defaultServerConfig conf server `E.catch` ignore)
                        `E.finally` putMVar ended ()
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            -- As above, but the stream's window is wide open: the DATA
            -- waits for the connection window alone.
            r <- runTCPClient host port $ \s -> do
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 $ B.pack [0, 4, 0x7f, 0xff, 0xff, 0xff]
                sendAll s $ frame 1 5 1 getHeaderBlock
                timeout 6000000 $ takeMVar ended
            r `shouldBe` Just ()
    it "gives up on a peer which never sends the preface" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse = sendResponse (responseNoBody ok200 []) []
            -- Masked so that no exception can reach it: it has to finish
            -- by itself, without the watchdog's last resort.
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 serverTimeout) freeSimpleConfig $ \conf ->
                    E.uninterruptibleMask_ (E.try $ run defaultServerConfig conf server)
                        >>= putMVar ended
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            r <- runTCPClient host port $ \_ -> timeout 6000000 $ takeMVar ended
            case r of
                Just (Left ConnectionIsTimeout) -> return ()
                _ -> expectationFailure $ "Unexpected result: " ++ show r
    it "stops a receiver which cannot be interrupted once it has gone away" $ do
        ended <- newEmptyMVar
        goVar <- newTVarIO False
        let server _req _aux sendResponse = sendResponse (responseNoBody ok200 []) []
            -- A read no exception reaches, as on a platform where a blocked
            -- recv cannot be interrupted; the one which gives up still can.
            uninterruptible conf = conf{confReadN = E.uninterruptibleMask_ . confReadN conf}
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 30000000) freeSimpleConfig $ \conf ->
                    (go (uninterruptible conf) `E.catch` ignore) `E.finally` putMVar ended ()
            go conf = runWith (readTVar goVar >>= check) defaultServerConfig conf server
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            -- The client says nothing after its SETTINGS and does not answer
            -- the PING, so the receiver waits in a read when it is stopped.
            r <- runTCPClient host port $ \s -> do
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 ""
                threadDelay 100000
                atomically $ writeTVar goVar True
                timeout 5000000 $ takeMVar ended
            r `shouldBe` Just ()
    it "stops a receiver which cannot be interrupted once the sender has failed" $ do
        ended <- newEmptyMVar
        let server _req _aux sendResponse = sendResponse (responseNoBody ok200 []) []
            -- No exception reaches the reads, and every write fails: the
            -- receiver used to be cancelled, which waited for good.
            broken conf =
                conf
                    { confReadN = E.uninterruptibleMask_ . confReadN conf
                    , confReadNWith = fmap (\r stop -> E.uninterruptibleMask_ . r stop) (confReadNWith conf)
                    , confSendAllWith = Just $ \_ _ -> E.throwIO $ userError "broken"
                    }
            runServer = runTCPServer (Just host) port $ \s ->
                E.bracket (allocSimpleConfig' s 32768 30000000) freeSimpleConfig $ \conf ->
                    (run defaultServerConfig (broken conf) server `E.catch` ignore)
                        `E.finally` putMVar ended ()
        E.bracket (forkIO runServer) killThread $ \_ -> do
            threadDelay 10000
            r <- runTCPClient host port $ \s -> do
                sendAll s connectionPreface
                sendAll s $ frame 4 0 0 ""
                timeout 5000000 $ takeMVar ended
            r `shouldBe` Just ()
  where
    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

-- | A frame of this type, flags and stream with this payload.
frame :: Word8 -> Word8 -> Word8 -> ByteString -> ByteString
frame typ flags sid payload =
    B.pack [0, fromIntegral (len `div` 256), fromIntegral (len `mod` 256), typ, flags, 0, 0, 0, sid]
        <> payload
  where
    len = B.length payload

-- | GET / for 127.0.0.1, from the static table and one literal.
getHeaderBlock :: ByteString
getHeaderBlock = B.pack [0x82, 0x86, 0x84, 0x01, 0x09] <> "127.0.0.1"

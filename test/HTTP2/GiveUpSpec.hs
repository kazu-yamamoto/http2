{-# LANGUAGE OverloadedStrings #-}

module HTTP2.GiveUpSpec (spec) where

import Control.Concurrent
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

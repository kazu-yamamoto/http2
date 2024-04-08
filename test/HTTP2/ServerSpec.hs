{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HTTP2.ServerSpec where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HTTP.Semantics
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString
import System.IO
import System.IO.Unsafe
import System.Random
import Test.Hspec

import Network.HPACK
import Network.HPACK.Internal
import qualified Network.HTTP2.Client as C
import qualified Network.HTTP2.Client.Internal as C
import Network.HTTP2.Frame
import Network.HTTP2.Server

port :: String
port = show $ unsafePerformIO (randomPort <$> getStdGen)
  where
    randomPort = fst . randomR (43124 :: Int, 44320)

host :: String
host = "127.0.0.1"

spec :: Spec
spec = do
    describe "server" $ do
        it "handles normal cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                (runClient allocSimpleConfig)

        it "should always send the connection preface first" $ do
            prefaceVar <- newEmptyMVar
            E.bracket (forkIO (runFakeServer prefaceVar)) killThread $ \_ -> do
                threadDelay 10000
                E.catch (runClient allocSlowPrefaceConfig) ignoreHTTP2Error

            preface <- takeMVar prefaceVar
            preface `shouldBe` connectionPreface

        it "prevents attacks" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runAttack rapidSettings `shouldThrow` connectionError "too many settings"
                runAttack rapidPing `shouldThrow` connectionError "too many ping"
                runAttack rapidEmptyHeader
                    `shouldThrow` connectionError "too many empty headers"
                runAttack rapidEmptyData `shouldThrow` connectionError "too many empty data"
                runAttack rapidRst `shouldThrow` connectionError "too many rst_stream"

ignoreHTTP2Error :: C.HTTP2Error -> IO ()
ignoreHTTP2Error _ = pure ()

runServer :: IO ()
runServer = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 32768)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf server)

runFakeServer :: MVar ByteString -> IO ()
runFakeServer prefaceVar = do
    runTCPServer (Just host) port $ \s -> do
        ref <- newIORef Nothing

        -- send settings
        sendAll s $
            "\x00\x00\x12\x04\x00\x00\x00\x00\x00"
                `mappend` "\x00\x03\x00\x00\x00\x80\x00\x04\x00"
                `mappend` "\x01\x00\x00\x00\x05\x00\xff\xff\xff"

        -- receive preface
        value <- defaultReadN s ref (B.length connectionPreface)
        putMVar prefaceVar value

        -- send goaway frame
        sendAll s "\x00\x00\x08\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01"

        -- wait for a few ms to make sure the client has a chance to close the
        -- socket on its end
        threadDelay 10000

server :: Server
server req _aux sendResponse = case requestMethod req of
    Just "GET" -> case requestPath req of
        Just "/" -> sendResponse responseHello []
        Just "/stream" -> sendResponse responseInfinite []
        Just "/push" -> do
            let pp = pushPromise "/push-pp" responsePP 0
            sendResponse responseHello [pp]
        _ -> sendResponse response404 []
    Just "POST" -> case requestPath req of
        Just "/echo" -> sendResponse (responseEcho req) []
        _ -> sendResponse responseHello []
    _ -> sendResponse response405 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

responsePP :: Response
responsePP = responseBuilder ok200 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("x-push", "True")
        ]
    body = byteString "Push\n"

responseInfinite :: Response
responseInfinite = responseStreaming ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body :: (Builder -> IO ()) -> IO () -> IO ()
    body write flush = do
        let go n = write (byteString (C8.pack (show n)) `mappend` "\n") *> flush *> go (succ n)
        go (0 :: Int)

response404 :: Response
response404 = responseNoBody notFound404 []

response405 :: Response
response405 = responseNoBody methodNotAllowed405 []

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    mhx = getFieldValue (toToken "X-Tag") (snd (requestHeaders req))
    streamingBody write _flush = do
        loop
        mt <- getRequestTrailers req
        firstTrailerValue <$> mt `shouldBe` mhx
      where
        loop = do
            bs <- getRequestBodyChunk req
            when (bs /= "") $ do
                void $ write $ byteString bs
                loop
    maker = trailersMaker (CH.hashInit :: Context SHA1)

-- Strictness is important for Context.
trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
  where
    !sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    !ctx' = CH.hashUpdate ctx bs

runClient :: (Socket -> BufferSize -> IO Config) -> IO ()
runClient allocConfig =
    runTCPClient host port $ runHTTP2Client
  where
    auth = host
    cliconf = C.defaultClientConfig{C.authority = auth}
    runHTTP2Client s =
        E.bracket
            (allocConfig s 4096)
            freeSimpleConfig
            (\conf -> C.run cliconf conf client)

    client :: C.Client ()
    client sendRequest aux =
        foldr1 concurrently_ $
            [ client0 sendRequest aux
            , client1 sendRequest aux
            , client2 sendRequest aux
            , client3 sendRequest aux
            , client3' sendRequest aux
            , client3'' sendRequest aux
            , client4 sendRequest aux
            , client5 sendRequest aux
            ]

-- delay sending preface to be able to test if it is always sent first
allocSlowPrefaceConfig :: Socket -> BufferSize -> IO Config
allocSlowPrefaceConfig s size = do
    config <- allocSimpleConfig s size
    pure config{confSendAll = slowPrefaceSend (confSendAll config)}
  where
    slowPrefaceSend :: (ByteString -> IO ()) -> ByteString -> IO ()
    slowPrefaceSend orig chunk = do
        when (C8.pack "PRI" `C8.isPrefixOf` chunk) $ do
            threadDelay 10000
        orig chunk

client0 :: C.Client ()
client0 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
        fmap statusMessage (C.responseStatus rsp) `shouldBe` Just "OK"

client1 :: C.Client ()
client1 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/push-pp" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just notFound404

client2 :: C.Client ()
client2 sendRequest _aux = do
    let req = C.requestNoBody methodPut "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just methodNotAllowed405

client3 :: C.Client ()
client3 sendRequest _aux = do
    let hx = "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
        req0 =
            C.requestFile methodPost "/echo" [("X-Tag", hx)] $
                FileSpec "test/inputFile" 0 1012731
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt `shouldBe` Just hx
  where
    !maker = trailersMaker (CH.hashInit :: Context SHA1)

client3' :: C.Client ()
client3' sendRequest _aux = do
    let hx = "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
        req0 = C.requestStreaming methodPost "/echo" [("X-Tag", hx)] $ \write _flush -> do
            let sendFile h = do
                    bs <- B.hGet h 1024
                    when (bs /= "") $ do
                        write $ byteString bs
                        sendFile h
            withFile "test/inputFile" ReadMode sendFile
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt `shouldBe` Just hx
  where
    !maker = trailersMaker (CH.hashInit :: Context SHA1)

client3'' :: C.Client ()
client3'' sendRequest _axu = do
    let hx = "59f82dfddc0adf5bdf7494b8704f203a67e25d4a"
        req0 = C.requestStreaming methodPost "/echo" [("X-Tag", hx)] $ \write _flush -> do
            let chunk = C8.replicate (16384 * 2) 'c'
                tag = C8.replicate 16 't'
            -- I don't think 9 is important here, this is just what I have, the client hangs on receiving the last one
            replicateM_ 9 $ write $ byteString chunk
            write $ byteString tag
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt `shouldBe` Just hx
  where
    !maker = trailersMaker (CH.hashInit :: Context SHA1)

client4 :: C.Client ()
client4 sendRequest _aux = do
    let req0 = C.requestNoBody methodGet "/push" []
    sendRequest req0 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
    let req1 = C.requestNoBody methodGet "/push-pp" []
    sendRequest req1 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

client5 :: C.Client ()
client5 sendRequest _aux = do
    let req0 = C.requestNoBody methodGet "/stream" []
    sendRequest req0 $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200
        let go n
                | n > 0 = do
                    _ <- C.getResponseBodyChunk rsp
                    go (pred n)
                | otherwise = pure ()
        go (100 :: Int)

firstTrailerValue :: TokenHeaderTable -> FieldValue
firstTrailerValue = snd . Prelude.head . fst

runAttack :: (C.ClientIO -> IO ()) -> IO ()
runAttack attack =
    runTCPClient host port $ runHTTP2Client
  where
    auth = host
    cliconf = C.defaultClientConfig{C.authority = auth}
    runHTTP2Client s =
        E.bracket
            (allocSimpleConfig s 4096)
            freeSimpleConfig
            (\conf -> C.runIO cliconf conf client)
    client cconf = return $ do
        attack cconf
        threadDelay 1000000

rapidSettings :: C.ClientIO -> IO ()
rapidSettings C.ClientIO{..} = do
    let einfo = EncodeInfo defaultFlags 0 Nothing
        bs = encodeFrame einfo $ SettingsFrame [(SettingsEnablePush, 0)]
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs

rapidPing :: C.ClientIO -> IO ()
rapidPing C.ClientIO{..} = do
    let einfo = EncodeInfo defaultFlags 0 Nothing
        opaque64 = "01234567"
        bs = encodeFrame einfo $ PingFrame opaque64
    replicateM_ 20 $ cioWriteBytes bs

rapidEmptyHeader :: C.ClientIO -> IO ()
rapidEmptyHeader C.ClientIO{..} = do
    (sid, _) <- cioCreateStream
    let einfo = EncodeInfo defaultFlags sid Nothing
        bs = encodeFrame einfo $ HeadersFrame Nothing ""
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs
    cioWriteBytes bs

rapidEmptyData :: C.ClientIO -> IO ()
rapidEmptyData C.ClientIO{..} = do
    (sid, _) <- cioCreateStream
    let einfoH = EncodeInfo (setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/")
                , (":method", "GET")
                ]
        bsH = encodeFrame einfoH $ HeadersFrame Nothing hdr
    cioWriteBytes bsH
    let einfoD = EncodeInfo defaultFlags sid Nothing
        bsD = encodeFrame einfoD $ DataFrame ""
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD
    cioWriteBytes bsD

rapidRst :: C.ClientIO -> IO ()
rapidRst C.ClientIO{..} = do
    reset
    reset
    reset
    reset
    reset
    reset
    reset
    reset
  where
    reset = do
        (sid, _) <- cioCreateStream
        -- setEndStream for HalfClosedRemote
        let einfoH = EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing
            hdr =
                hpackEncode
                    [ (":scheme", "http")
                    , (":authority", "127.0.0.1")
                    , (":path", "/")
                    , (":method", "GET")
                    ]
            bsH = encodeFrame einfoH $ HeadersFrame Nothing hdr
        cioWriteBytes bsH
        let einfoR = EncodeInfo defaultFlags sid Nothing
            -- Only (HalfClosedRemote, NoError) is accepted.
            -- Otherwise, a stream error terminates the connection.
            bsR = encodeFrame einfoR $ RSTStreamFrame NoError
        cioWriteBytes bsR

connectionError :: C.ReasonPhrase -> C.HTTP2Error -> Bool
connectionError phrase (C.ConnectionErrorIsReceived _ _ p)
    | phrase == p = True
connectionError _ _ = False

hpackEncode :: [(ByteString, ByteString)] -> ByteString
hpackEncode kvs = foldr cat "" kvs
  where
    (k, v) `cat` b =
        B.singleton 0x10
            <> unsafePerformIO (encodeInteger 7 (B.length k))
            <> k
            <> unsafePerformIO (encodeInteger 7 (B.length v))
            <> v
            <> b

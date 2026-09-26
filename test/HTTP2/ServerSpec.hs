{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HTTP2.ServerSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Crypto.Hash (Context, SHA1)
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Data.Maybe (isNothing)
import Network.HTTP.Semantics
import Network.HTTP.Types
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString
import System.IO
import System.IO.Unsafe
import System.Random
import System.Timeout (timeout)
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
        it "sends a header block and trailers larger than a frame" $
            -- Both have to go out as HEADERS and CONTINUATION frames and be
            -- put back together on receipt; the requests after them check
            -- that the two ends' HPACK tables still agree.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                r <- timeout 5000000 $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                            sendRequest (C.requestNoBody methodGet "/big" []) $ \rsp -> do
                                getFieldValue (toToken "x-big") (snd (C.responseHeaders rsp))
                                    `shouldBe` Just bigVal
                                let drain = do
                                        bs <- C.getResponseBodyChunk rsp
                                        unless (B.null bs) drain
                                drain
                                mt <- C.getResponseTrailers rsp
                                (mt >>= getFieldValue (toToken "x-big-trailer") . snd)
                                    `shouldBe` Just bigVal
                            -- Same connection: the HPACK state must still agree.
                            forM_ [1 :: Int, 2] $ \_ ->
                                sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                                    C.responseStatus rsp `shouldBe` Just ok200
                r `shouldBe` Just ()

        it "handles normal cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient allocSimpleConfig

        it "delivers 103 Early Hints to the client's informational handler" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                hintsRef <- newIORef []
                runClientEarly hintsRef >>= (`shouldBe` Just ok200)
                hints <- readIORef hintsRef
                map (getFieldValue (toToken "link") . snd) hints
                    `shouldBe` [ Just "</style.css>; rel=preload; as=style"
                               , Just "</app.js>; rel=preload; as=script"
                               ]

        it "should always send the connection preface first" $ do
            prefaceVar <- newEmptyMVar
            E.bracket (forkIO (runFakeServer prefaceVar)) killThread $ \_ -> do
                threadDelay 10000
                E.catch (runClient allocSlowPrefaceConfig) ignoreHTTP2Error

            preface <- takeMVar prefaceVar
            preface `shouldBe` connectionPreface

        it "refuses one stream over the limit and keeps the connection" $
            E.bracket (forkIO runServerMaxConc1) killThread $ \_ -> do
                threadDelay 10000
                -- The server announced room for one concurrent stream.  Open
                -- one, reset it, then open two more: the second of those is
                -- the one over the limit.
                --
                -- Two things are on trial.  That the reset gives the slot back
                -- exactly once -- decrementing the count twice, as it used to,
                -- would leave room for both.  And that being over the limit
                -- costs you that stream and not the connection: no GOAWAY.
                frames <-
                    rawExchange
                        [ openStreamFrame 1
                        , encodeFrame (EncodeInfo defaultFlags 1 Nothing) $
                            RSTStreamFrame Cancel
                        , openStreamFrame 3
                        , openStreamFrame 5
                        ]
                [(sid, ec) | (FrameRSTStream, sid, ec) <- resets frames]
                    `shouldBe` [(5, RefusedStream)]
                [() | (FrameGoAway, _, _) <- resets frames] `shouldBe` []

        it "releases a worker whose stream the peer reset" $ do
            doneVar <- newEmptyMVar
            E.bracket (forkIO (runServerCancel doneVar)) killThread $ \_ -> do
                threadDelay 10000
                runAttack cancelInFlight
                timeout 1000000 (takeMVar doneVar) `shouldReturn` Just ()

        it "survives a padded HEADERS whose padding covers the priority fields" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runAttack paddingOverPriority
                    `shouldThrow` connectionError "no room for priority fields"

        it "resets one stream and goes on serving the connection" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runStreamErrorClient

        it "limits the resets a peer can make us send (MadeYouReset)" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                -- Not through the client library: it would take the
                -- server's first RST_STREAM, on a stream it never opened
                -- itself, for a protocol error of its own.
                timeout 5000000 rapidStreamError
                    `shouldReturn` Just (Just (EnhanceYourCalm, "too many stream errors"))

        it "gives back the slot of a stream both sides streamed on" $
            -- Room for four concurrent streams, so a few slots that are
            -- never given back stop the connection within a few thousand
            -- requests one after another.  Both sides stream with flushes: the receiver used to write back a stream state
            -- it had read before the sender half-closed the stream, undoing
            -- the half-close, so the peer's END_STREAM then left the stream
            -- half-closed instead of closed and in the table for good.
            --
            -- It is a race between the receiver and the sender, so it needs
            -- them running in parallel: on one capability it hardly ever
            -- shows.
            withCapabilities 4 $
                E.bracket (forkIO runServerSmallWindow) killThread $ \_ -> do
                    threadDelay 10000
                    done <- newIORef (0 :: Int)
                    r <- timeout 60000000 $ runTCPClient host port $ \s -> do
                        -- Fifty small writes each way per request: without
                        -- this, Nagle and delayed ACKs can hold each one up.
                        setSocketOption s NoDelay 1
                        E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                            C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ ->
                                forM_ [1 .. 2000 :: Int] $ \_ -> do
                                    let req = C.requestStreaming methodPost "/both" [] $ \write flush ->
                                            replicateM_ 50 $ write (byteString (C8.replicate 50 'a')) >> flush
                                    sendRequest req $ \rsp -> do
                                        let drain n = do
                                                bs <- C.getResponseBodyChunk rsp
                                                if B.null bs then return n else drain (n + B.length bs)
                                        drain 0 `shouldReturn` 2500
                                    modifyIORef' done (+ 1)
                    -- How far it got tells a hang from a slow run.
                    n <- readIORef done
                    when (isNothing r) $
                        expectationFailure $
                            "timed out after " ++ show n ++ " of 2000 requests"

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

-- | Like 'runServer', but announcing room for a single concurrent stream.
-- | Running with at least this many capabilities.
withCapabilities :: Int -> IO a -> IO a
withCapabilities n act =
    E.bracket getNumCapabilities setNumCapabilities $ \old -> do
        setNumCapabilities (max n old)
        act

-- | Room for four concurrent streams and a small window, so that WINDOW_UPDATE
-- frames go back and forth all the time.
runServerSmallWindow :: IO ()
runServerSmallWindow = runTCPServer (Just host) port runHTTP2Server
  where
    sconf =
        defaultServerConfig
            { settings =
                (settings defaultServerConfig)
                    { maxConcurrentStreams = Just 4
                    , initialWindowSize = 8192
                    }
            }
    runHTTP2Server s = do
        setSocketOption s NoDelay 1
        E.bracket
            (allocSimpleConfig s 32768)
            freeSimpleConfig
            (\conf -> run sconf conf server)

runServerMaxConc1 :: IO ()
runServerMaxConc1 = runTCPServer (Just host) port runHTTP2Server
  where
    sconf =
        defaultServerConfig
            { settings = (settings defaultServerConfig){maxConcurrentStreams = Just 1}
            }
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 32768)
            freeSimpleConfig
            (\conf -> run sconf conf server)

-- | A server whose handler waits long enough for a RST_STREAM to arrive
-- before it responds, and then signals that 'sendResponse' returned.
runServerCancel :: MVar () -> IO ()
runServerCancel doneVar = runTCPServer (Just host) port runHTTP2Server
  where
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 32768)
            freeSimpleConfig
            (\conf -> run defaultServerConfig conf cancelServer)
    cancelServer _req _aux sendResponse = do
        threadDelay 200000
        sendResponse responseHello []
        putMVar doneVar ()

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
server req aux sendResponse = case requestMethod req of
    Just "GET" -> case requestPath req of
        Just "/" -> sendResponse responseHello []
        Just "/early" -> do
            auxSendInformational
                aux
                earlyHints103
                [("link", "</style.css>; rel=preload; as=style")]
            auxSendInformational
                aux
                earlyHints103
                [("link", "</app.js>; rel=preload; as=script")]
            sendResponse responseHello []
        Just "/stream" -> sendResponse responseInfinite []
        Just "/big" -> sendResponse responseBig []
        Just "/push" -> do
            let pp = pushPromise "/push-pp" responsePP 0
            sendResponse responseHello [pp]
        _ -> sendResponse response404 []
    Just "POST" -> case requestPath req of
        Just "/echo" -> sendResponse (responseEcho req) []
        Just "/both" -> do
            -- Read the body on the side, so that the response does not
            -- wait for it.
            _ <-
                forkIO $
                    let d = getRequestBodyChunk req >>= \bs -> unless (B.null bs) d
                     in d
            sendResponse responseBoth []
        _ -> sendResponse responseHello []
    _ -> sendResponse response405 []

-- | Larger than the default frame size and than the server's 32K buffer.
bigVal :: ByteString
bigVal = C8.replicate 40000 'x'

responseBig :: Response
responseBig = setResponseTrailersMaker rsp maker
  where
    rsp = responseBuilder ok200 [("x-big", bigVal)] "hello"
    maker Nothing = return $ Trailers [("x-big-trailer", bigVal)]
    maker (Just _) = return $ NextTrailersMaker maker

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

earlyHints103 :: Status
earlyHints103 = mkStatus 103 "Early Hints"

responsePP :: Response
responsePP = responseBuilder ok200 header body
  where
    header =
        [ ("Content-Type", "text/plain")
        , ("x-push", "True")
        ]
    body = byteString "Push\n"

-- | A streaming response that does not wait for the request body, so that
-- both ends are sending at once and either can finish first.
responseBoth :: Response
responseBoth = responseStreaming ok200 [] $ \write flush ->
    replicateM_ 50 $ write (byteString (C8.replicate 50 'b')) >> flush

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

-- | Request @/early@ with an informational handler installed, recording each
-- 103 Early Hints section and returning the final response status.
runClientEarly :: IORef [TokenHeaderTable] -> IO (Maybe Status)
runClientEarly hintsRef = runTCPClient host port $ \s ->
    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf0 ->
        C.run cliconf (conf0{confOnInformational = onInformational}) $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/early" []) (return . C.responseStatus)
  where
    cliconf = C.defaultClientConfig{C.authority = host}
    onInformational _sid tbl = modifyIORef' hintsRef (++ [tbl])

runClient :: (Socket -> BufferSize -> IO Config) -> IO ()
runClient allocConfig =
    runTCPClient host port runHTTP2Client
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
        foldr1
            concurrently_
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
        let consumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") consumeBody
        consumeBody
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
        let consumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") consumeBody
        consumeBody
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
        let consumeBody = do
                bs <- C.getResponseBodyChunk rsp
                when (bs /= "") consumeBody
        consumeBody
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
firstTrailerValue tbl = case fst tbl of
    [] -> error "firstTrailerValue"
    x : _ -> snd x

runAttack :: (C.ClientIO -> IO ()) -> IO ()
runAttack attack =
    runTCPClient host port runHTTP2Client
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

-- | MadeYouReset (CVE-2025-8671): the same churn as 'rapidRst' without a
-- single RST_STREAM from us.  Each stream gets a handler that goes on
-- running, then a PRIORITY making it depend on itself, which the server
-- answers by resetting the stream -- giving its concurrency slot back while
-- the handler runs on.  Those resets did not count against the limit on
-- resets, so this could be kept up for as long as the peer liked.
rapidStreamError :: IO (Maybe (ErrorCode, ByteString))
rapidStreamError = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    forM_ [1, 3 .. 15] $ \sid -> do
        let einfoH = EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing
            hdr =
                hpackEncode
                    [ (":scheme", "http")
                    , (":authority", "127.0.0.1")
                    , (":path", "/stream")
                    , (":method", "GET")
                    ]
            einfoP = EncodeInfo defaultFlags sid Nothing
        sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
        sendAll s $ encodeFrame einfoP $ PriorityFrame $ Priority False sid 16
    awaitGoAway s
  where
    -- What the server says in its GOAWAY, if it sends one before closing.
    awaitGoAway s = do
        mh <- recvExactly s frameHeaderLength
        case mh of
            Nothing -> return Nothing
            Just h -> do
                let (ftyp, fh) = decodeFrameHeader h
                mp <- recvExactly s $ payloadLength fh
                case (ftyp, mp) of
                    (FrameGoAway, Just p)
                        | Right (GoAwayFrame _ err msg) <- decodeGoAwayFrame fh p ->
                            return $ Just (err, msg)
                    (_, Just _) -> awaitGoAway s
                    _ -> return Nothing
    recvExactly s n = go n []
      where
        go 0 acc = return $ Just $ B.concat $ reverse acc
        go k acc = do
            bs <- recv s k
            if B.null bs
                then return Nothing
                else go (k - B.length bs) (bs : acc)

-- | Open a stream, reset it, then open two more.  The server announced room
-- for one concurrent stream, so the third one here must be refused.
--
-- Closing a stream used to give its slot back twice -- a RST_STREAM carrying a
-- non-critical error code is closed by both 'stream' and 'processState' -- so
-- the count drifted down by one on every reset and this sequence went through
-- unchallenged.
-- | A HEADERS frame opening a stream and leaving it open, so that it goes on
-- holding a concurrency slot.
--
-- Stream identifiers are written out rather than taken from
-- 'C.cioCreateStream': the limit being overrun is the one the server
-- announced, and asking for a stream the proper way would block on that same
-- limit on this side.
openStreamFrame :: StreamId -> ByteString
openStreamFrame sid = encodeFrame einfo $ HeadersFrame Nothing hdr
  where
    einfo = EncodeInfo (setEndHeader defaultFlags) sid Nothing
    hdr =
        hpackEncode
            [ (":scheme", "http")
            , (":authority", "127.0.0.1")
            , (":path", "/")
            , (":method", "GET")
            ]

-- | Speak raw frames to the server and collect what it says back.
rawExchange :: [ByteString] -> IO [(FrameType, StreamId, ByteString)]
rawExchange out = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    mapM_ (sendAll s) out
    splitFrames <$> collect mempty s
  where
    collect acc s = do
        mbs <- timeout 300000 $ recv s 4096
        case mbs of
            Just bs | not (B.null bs) -> collect (acc `B.append` bs) s
            _ -> return acc

splitFrames :: ByteString -> [(FrameType, StreamId, ByteString)]
splitFrames bs
    | B.length bs < frameHeaderLength = []
    | otherwise =
        let (h, rest) = B.splitAt frameHeaderLength bs
            (typ, FrameHeader{payloadLength, streamId}) = decodeFrameHeader h
            (body, rest') = B.splitAt payloadLength rest
         in (typ, streamId, body) : splitFrames rest'

-- | The RST_STREAM and GOAWAY frames among them, with their error codes.
resets
    :: [(FrameType, StreamId, ByteString)] -> [(FrameType, StreamId, ErrorCode)]
resets frames =
    [ (typ, sid, ec)
    | (typ, sid, body) <- frames
    , typ == FrameRSTStream || typ == FrameGoAway
    , Just ec <- [errorCodeOf typ sid body]
    ]
  where
    errorCodeOf FrameRSTStream sid body =
        case decodeRSTStreamFrame (FrameHeader (B.length body) defaultFlags sid) body of
            Right (RSTStreamFrame ec) -> Just ec
            _ -> Nothing
    errorCodeOf FrameGoAway sid body =
        case decodeGoAwayFrame (FrameHeader (B.length body) defaultFlags sid) body of
            Right (GoAwayFrame _ ec _) -> Just ec
            _ -> Nothing
    errorCodeOf _ _ _ = Nothing

-- | Open a stream and cancel it straight away, while the server is still
-- working on the response.
--
-- The sender skips a stream that is already half-closed, and used to return
-- without telling the thread that enqueued the output.  That thread sat in
-- 'syncWithSender'' on an MVar nothing would fill, so 'sendResponse' never
-- returned and the worker was only reclaimed when the timeout manager killed
-- it, seconds later.
cancelInFlight :: C.ClientIO -> IO ()
cancelInFlight C.ClientIO{..} = do
    -- setEndStream for HalfClosedRemote, so that CANCEL is accepted as a
    -- stream error rather than taken down the connection.
    let einfoH = EncodeInfo (setEndStream $ setEndHeader defaultFlags) 1 Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/")
                , (":method", "GET")
                ]
    cioWriteBytes $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    cioWriteBytes $
        encodeFrame (EncodeInfo defaultFlags 1 Nothing) $
            RSTStreamFrame Cancel

-- | Send a malformed request, then a good one down the same connection.
--
-- RFC 9113 section 8.1.1 makes a malformed request a stream error, so the
-- server must reset that one stream and keep serving: the second request is
-- the point of the test.  The whole connection used to come down with the
-- first, taking every other stream on it along.
runStreamErrorClient :: IO ()
runStreamErrorClient = runTCPClient host port $ \s ->
    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
        C.run cliconf conf $ \sendRequest _aux -> do
            -- "te" may only ever be "trailers" (section 8.2.2), and unlike
            -- "connection" it is not one of the headers the sender strips.
            let bad = C.requestNoBody methodGet "/" [("te", "gzip")]
            sendRequest bad (\_ -> return ()) `shouldThrow` streamWasReset
            let good = C.requestNoBody methodGet "/" []
            sendRequest good $ \rsp ->
                C.responseStatus rsp `shouldBe` Just ok200
  where
    cliconf = C.defaultClientConfig{C.authority = host}

streamWasReset :: Selector C.HTTP2Error
streamWasReset C.StreamResetIsReceived{} = True
streamWasReset _ = False

-- | A HEADERS frame with PADDED and PRIORITY set, six octets of payload and a
-- Pad Length of five, so that the padding covers the whole of the priority
-- fields the flag promises.
--
-- Six octets is the smallest payload the frame header check accepts for those
-- two flags together, so this gets through it; the decoder then took the five
-- priority octets out of what padding had left empty, reading off the end of
-- the buffer.  The empty ByteString is the shared one, whose pointer is null,
-- so what died was the process rather than the connection.
paddingOverPriority :: C.ClientIO -> IO ()
paddingOverPriority C.ClientIO{..} = do
    let flags = setPadded $ setPriority $ setEndHeader defaultFlags
        header = encodeFrameHeader FrameHeaders $ FrameHeader 6 flags 1
        payload = B.pack [5, 0, 0, 0, 0, 0] -- Pad Length 5, then the padding
    cioWriteBytes $ header `B.append` payload

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

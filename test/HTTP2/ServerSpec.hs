{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- GHC 9.12 and later (still on master), with -O, compile 'responseInfinite'
-- to a response with no body: 'OutBodyNone' instead of 'OutBodyStreaming'.
-- Full laziness floats the constructor application to a top-level thunk,
-- and that thunk reaches code that switches on the pointer tag without
-- evaluating it: https://gitlab.haskell.org/ghc/ghc/-/work_items/27857
-- The "infinite" stream then ends with its HEADERS, and the MadeYouReset
-- test sometimes sees a stream closed before its PRIORITY arrives (#191).
-- 9.10 and earlier are not affected.
{-# OPTIONS_GHC -fno-full-laziness #-}

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
import Data.Maybe (isJust, isNothing)
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

        it "accepts a content-length on a response with no content" $
            -- RFC 9113, section 8.1.1: the response to HEAD, 204 and 304 can
            -- carry a non-zero content-length without content.  The client
            -- used to take each of these for a malformed response.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                            let noContent method path =
                                    sendRequest (C.requestNoBody method path []) $ \rsp -> do
                                        C.responseStatus rsp `shouldSatisfy` isJust
                                        C.getResponseBodyChunk rsp `shouldReturn` ""
                            noContent methodHead "/"
                            noContent methodHead "/data"
                            noContent methodGet "/not-modified"
                            -- A response that is meant to have content still
                            -- has to match its content-length.
                            sendRequest (C.requestNoBody methodGet "/no-content" []) (const $ return ())
                                `shouldThrow` malformedResponse

        it "does not open a stream for a PRIORITY frame" $
            -- Over a raw socket, as the client library does not send
            -- PRIORITY.  The server allows 64 concurrent streams; each of
            -- these PRIORITY frames used to open one and hold its slot,
            -- so the request after them was refused.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 idlePriority `shouldReturn` Just (Just "HEADERS")

        it "closes the connection when SETTINGS overflow a stream's window" $
            -- RFC 9113, section 6.9.2: a connection error of type
            -- FLOW_CONTROL_ERROR.  The overflow is found in the sender,
            -- which used to stop on it without a word, leaving the
            -- connection open and silent.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                -- A connection error: GOAWAY, with no RST_STREAM before it.
                timeout 5000000 settingsOverflow
                    `shouldReturn` Just (False, Just FlowControlError)

        it "accepts empty trailers" $
            -- A HEADERS frame with END_STREAM and an empty field block ends
            -- the body with no trailer fields.  The empty block used to be
            -- taken for a truncated one: COMPRESSION_ERROR, and the
            -- connection closed.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 emptyTrailers `shouldReturn` Just (Just "HEADERS")

        it "checks a padded body against its content-length" $
            -- Padding is not content (RFC 9113, section 6.1).  It used to
            -- be counted into the body's length, so a padded body that
            -- matched its content-length was reset as one that did not.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 paddedBody `shouldReturn` Just (Just "DATA 4")

        it "gives the padding of a body back to the windows" $
            -- 2000 DATA frames of one octet of content and 255 of padding:
            -- about twice the stream's window.  The padding used to be
            -- charged and never given back, so a peer keeping to the
            -- windows stalled, and one that did not, like this one, broke
            -- the stream's limit and had the connection closed.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 paddingWindow `shouldReturn` Just (Just "DATA 2000")

        it "gives DATA it refuses back to the connection window" $
            -- DATA on a stream the peer has half-closed is a stream error
            -- (RFC 9113, section 5.1), but it still counts against the
            -- connection window (section 6.9).  It used to be left out, so
            -- the peer's view of that window shrank for good.  With a
            -- window of 65535, the refused 16384 octets and the 16384 of
            -- the next request make up the half that is given back.
            E.bracket (forkIO runServerSmallConnWindow) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 refusedData `shouldReturn` Just (Just (32768, "16384"))

        it "goes on sending requests after one fails before it is queued" $
            -- The file of this requestFile does not exist, so the request
            -- fails after its stream id is taken and before it is queued.
            -- Requests are queued in stream id order, so every one after it
            -- used to wait for its turn for ever.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                r <- timeout 5000000 $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                            let missing =
                                    C.requestFile methodPost "/echo" [] $
                                        FileSpec "test/no-such-file" 0 10
                            failed <- E.try $ sendRequest missing (const $ return ())
                            either (const True) (const False) (failed :: Either E.SomeException ())
                                `shouldBe` True
                            replicateM_ 3 $
                                sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                                    C.responseStatus rsp `shouldBe` Just ok200
                r `shouldBe` Just ()

        it "answers without a push when the peer has no room for one" $
            -- SETTINGS_MAX_CONCURRENT_STREAMS of 0 is how a peer can refuse
            -- pushes (RFC 9113, section 8.4).  The push of /push-pp used to
            -- wait for room for ever, and the response to /push with it.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 pushNoRoom `shouldReturn` Just (Just "HEADERS")

        it "frees the stream of a response the client did not read to the end" $
            -- /endless never ends.  Each request here reads one chunk of it
            -- and is done, by returning or by throwing.  Its stream used to
            -- stay open, holding one of the server's 64 slots, with what the
            -- server sent never given back to the connection window: the
            -- 65th request waited for a slot for ever.  Now each is reset,
            -- 70 in a burst, which takes a server allowing more resets a
            -- second than the default.
            E.bracket (forkIO runServerManyResets) killThread $ \_ -> do
                threadDelay 10000
                r <- timeout 10000000 $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                            forM_ [1 .. 70 :: Int] $ \i -> do
                                let abandon rsp = do
                                        _ <- C.getResponseBodyChunk rsp
                                        when (even i) $ E.throwIO $ userError "done with it"
                                r <- E.try $ sendRequest (C.requestNoBody methodGet "/endless" []) abandon
                                either (\e -> const (return ()) (e :: E.IOException)) return r
                            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                                C.responseStatus rsp `shouldBe` Just ok200
                r `shouldBe` Just ()

        it "goes on when pushes nobody asks for fill the connection window" $
            -- Each /push-big comes with a push of 20000 octets that is never
            -- asked for.  With a connection window of 65535, the fourth push
            -- used to find it used up by the first three, unread, and the
            -- connection stalled, the responses to /push-big with it.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                let cconf =
                        C.defaultClientConfig
                            { C.authority = host
                            , C.connectionWindowSize = defaultWindowSize
                            }
                r <- timeout 5000000 $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run cconf conf $ \sendRequest _ ->
                            replicateM_ 10 $
                                sendRequest (C.requestNoBody methodGet "/push-big" []) $ \rsp -> do
                                    C.responseStatus rsp `shouldBe` Just ok200
                                    let body = do
                                            bs <- C.getResponseBodyChunk rsp
                                            unless (B.null bs) body
                                    body
                r `shouldBe` Just ()

        it "counts streams whose handlers are running in its GOAWAY" $
            -- RFC 9113, section 6.8: the last stream identifier is the
            -- highest one that "might have been processed".  Streams 1 and
            -- 3 are being answered when the connection is closed, and used
            -- to be left out until their handlers had returned, so the
            -- GOAWAY said 0: as if the client could send them again.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 goAwayLastStream `shouldReturn` Just (Just (3, ProtocolError))

        it "finishes what the server will still answer after its GOAWAY" $
            -- RFC 9113, section 6.8: a GOAWAY with NO_ERROR and last stream 1
            -- says stream 1 will still be answered, stream 3 will not.  The
            -- client used to close the connection as soon as it came,
            -- failing both, and the client function with them.
            E.bracket (forkIO runGoAwayServer) killThread $ \_ -> do
                threadDelay 10000
                r <- timeout 5000000 $ E.try $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ -> do
                            let get = E.try . flip sendRequest readAll . C.requestNoBody methodGet "/" $ []
                                readAll rsp = do
                                    bs <- C.getResponseBodyChunk rsp
                                    if B.null bs then return "" else (bs <>) <$> readAll rsp
                            (r1, r3) <- concurrently get (threadDelay 50000 >> get)
                            -- By now the connection has run its course,
                            -- and the client function goes on: nothing new
                            -- goes out, and it is not killed either.
                            threadDelay 100000
                            r5 <- get
                            return (status r1, status r3, status r5)
                case r of
                    Just (Right rs) -> rs `shouldBe` ("hello", "closed", "closed")
                    Just (Left e) -> expectationFailure $ show (e :: C.HTTP2Error)
                    Nothing -> expectationFailure "timed out"

        it "answers the requests it has after the client's GOAWAY" $
            -- A client's GOAWAY speaks of the server's own streams, and
            -- the requests it has already sent are still to be answered.
            -- The server used to close the connection on it at once.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 goAwayFromClient
                    `shouldReturn` Just ["HEADERS 1", "DATA 1 END_STREAM", "GOAWAY NoError"]

        it "resets a malformed request and goes on serving the connection" $
            -- An upper-case field name makes the request malformed: a
            -- stream error (RFC 9113, section 8.1.1).  It used to close the
            -- connection, and the next request on it went unanswered.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 malformedRequest
                    `shouldReturn` Just ["RST_STREAM 1 ProtocolError", "HEADERS 3"]

        it "answers without a body while the connection window is shut" $
            -- HEADERS are not flow-controlled (RFC 9113, section 6.9).  The
            -- sender used to wait for the connection window before taking
            -- anything off its queue, so once /endless had used it up, the
            -- response to a request with no body never went out.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                timeout 5000000 shutWindow `shouldReturn` Just (Just "HEADERS 3, then DATA 1")

        it "sends a PUSH_PROMISE before the response that carries it" $
            -- /push answers with a push of /push-pp, so a request for
            -- /push-pp after it is served from the push.  The server used to
            -- let the response to /push overtake the PUSH_PROMISE now and
            -- then; the client then asked the server for /push-pp itself,
            -- and got 404.  One round in a few dozen did, so 200 of them --
            -- which also takes more pushes than the peer allows concurrent
            -- streams, so pushed streams that are never closed show too.
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                done <- newIORef (0 :: Int)
                r <- timeout 30000000 $ runTCPClient host port $ \s ->
                    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
                        C.run C.defaultClientConfig{C.authority = host} conf $ \sendRequest _ ->
                            replicateM_ 200 $ do
                                -- Bodies are read to the end, so that the
                                -- streams close and give their slots back.
                                let drain rsp = do
                                        bs <- C.getResponseBodyChunk rsp
                                        unless (B.null bs) $ drain rsp
                                sendRequest (C.requestNoBody methodGet "/push" []) $ \rsp -> do
                                    C.responseStatus rsp `shouldBe` Just ok200
                                    drain rsp
                                sendRequest (C.requestNoBody methodGet "/push-pp" []) $ \rsp -> do
                                    C.responseStatus rsp `shouldBe` Just ok200
                                    drain rsp
                                modifyIORef' done (+ 1)
                -- How far it got tells a hang (at 64, the peer's concurrency
                -- limit, if pushed streams leak) from a slow run.
                n <- readIORef done
                when (isNothing r) $
                    expectationFailure $
                        "timed out after " ++ show n ++ " of 200 rounds"

        it "uploads a file through runIO past the stream's window" $
            -- The server announces an 8192-octet window.  runIO put the rest
            -- of a body back on the queue without waiting for the window to
            -- open; with none left, the file was read into no room, and a
            -- read of 0 octets is the end of the file, so the request ended
            -- with END_STREAM after the first window's worth.
            E.bracket (forkIO runServerSmallWindow) killThread $ \_ -> do
                threadDelay 10000
                timeout 10000000 uploadIO `shouldReturn` Just 100000

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
-- | Uploading 100000 octets of a file through 'C.runIO', and what the server
-- says it received.
uploadIO :: IO Int
uploadIO = runTCPClient host port $ \s ->
    E.bracket (allocSimpleConfig s 4096) freeSimpleConfig $ \conf ->
        C.runIO C.defaultClientConfig{C.authority = host} conf $ \C.ClientIO{..} ->
            return $ do
                let body rsp acc = do
                        bs <- C.getResponseBodyChunk rsp
                        if B.null bs then return acc else body rsp (acc <> bs)
                    exchange req = cioWriteRequest req >>= cioReadResponse . snd
                -- A request first, so that the server's SETTINGS -- and its
                -- small window -- are known before the upload starts.
                _ <- exchange (C.requestNoBody methodGet "/" []) >>= (`body` "")
                rsp <-
                    exchange $
                        C.requestFile methodPost "/count" [] $
                            FileSpec "test/inputFile" 0 100000
                read . C8.unpack <$> body rsp ""

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

-- | Like 'runServer', but with the connection window left at its initial
-- 65535 octets.
runServerSmallConnWindow :: IO ()
runServerSmallConnWindow = runTCPServer (Just host) port runHTTP2Server
  where
    sconf = defaultServerConfig{connectionWindowSize = defaultWindowSize}
    runHTTP2Server s =
        E.bracket
            (allocSimpleConfig s 32768)
            freeSimpleConfig
            (\conf -> run sconf conf server)

-- | Like 'runServer', but allowing a client to reset 1000 streams a second.
runServerManyResets :: IO ()
runServerManyResets = runTCPServer (Just host) port runHTTP2Server
  where
    sconf =
        defaultServerConfig
            { settings = (settings defaultServerConfig){rstRateLimit = 1000}
            }
    runHTTP2Server s =
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

-- | Answering two requests with a GOAWAY that leaves out the second: the
-- headers of the first response, GOAWAY(NO_ERROR) with last stream 1, and
-- the rest of the first response a little later.
runGoAwayServer :: IO ()
runGoAwayServer = runTCPServer (Just host) port $ \s -> do
    _ <- recvAll s (B.length connectionPreface)
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let awaitRequests :: Int -> IO ()
        awaitRequests 2 = return ()
        awaitRequests n = do
            mf <- recvFrame s
            case mf of
                Nothing -> return ()
                Just (FrameSettings, fh, _)
                    | not (testAck (flags fh)) -> do
                        sendAll s $
                            encodeFrame (EncodeInfo (setAck defaultFlags) 0 Nothing) $
                                SettingsFrame []
                        awaitRequests n
                Just (FrameHeaders, _, _) -> awaitRequests (n + 1)
                Just _ -> awaitRequests n
    awaitRequests 0
    sendAll s $
        encodeFrame (EncodeInfo (setEndHeader defaultFlags) 1 Nothing) $
            HeadersFrame Nothing $
                hpackEncode [(":status", "200")]
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            GoAwayFrame 1 NoError "going"
    threadDelay 200000
    sendAll s $
        encodeFrame (EncodeInfo (setEndStream defaultFlags) 1 Nothing) $
            DataFrame "hello"
    -- Until the client closes it.
    let drain = recvFrame s >>= maybe (return ()) (const drain)
    drain

-- | How a request through the client ended: the body, or "closed" for
-- 'ConnectionIsClosed'.
status :: Either C.HTTP2Error ByteString -> ByteString
status (Right bs) = bs
status (Left C.ConnectionIsClosed) = "closed"
status (Left e) = C8.pack $ show e

-- | A request for /slow, then GOAWAY(NO_ERROR) at once.  What the server
-- sends from then on until it closes the connection.
goAwayFromClient :: IO [String]
goAwayFromClient = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    sendAll s $
        encodeFrame (EncodeInfo (setEndStream $ setEndHeader defaultFlags) 1 Nothing) $
            HeadersFrame Nothing $
                hpackEncode
                    [ (":scheme", "http")
                    , (":authority", "127.0.0.1")
                    , (":path", "/slow")
                    , (":method", "GET")
                    ]
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            GoAwayFrame 0 NoError "going"
    collect s
  where
    collect s = do
        mf <- recvFrame s
        case mf of
            Nothing -> return []
            Just (FrameHeaders, fh, _) -> (("HEADERS " ++ show (streamId fh)) :) <$> collect s
            Just (FrameData, fh, _)
                | testEndStream (flags fh) ->
                    (("DATA " ++ show (streamId fh) ++ " END_STREAM") :) <$> collect s
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    (("GOAWAY " ++ show err) :) <$> collect s
            Just _ -> collect s

server :: Server
server req aux sendResponse = case requestMethod req of
    Just "GET" -> case requestPath req of
        Just "/" -> sendResponse responseHello []
        -- A moment before the answer.
        Just "/slow" -> threadDelay 200000 >> sendResponse responseHello []
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
        -- Like /stream, but going quietly once the client resets it.
        Just "/endless" -> sendResponse responseEndless []
        Just "/not-modified" -> sendResponse (responseNoBody notModified304 bigLength) []
        -- Says it has content, and has none: malformed.
        Just "/no-content" -> sendResponse (responseNoBody ok200 bigLength) []
        Just "/big" -> sendResponse responseBig []
        Just "/push" -> do
            let pp = pushPromise "/push-pp" responsePP 0
            sendResponse responseHello [pp]
        -- A push of 20000 octets.
        Just "/push-big" -> do
            let pp = pushPromise "/push-big-pp" responsePushBig 0
            sendResponse responseHello [pp]
        _ -> sendResponse response404 []
    Just "POST" -> case requestPath req of
        Just "/echo" -> sendResponse (responseEcho req) []
        -- How many octets of body arrived.
        Just "/count" -> do
            let count n = do
                    bs <- getRequestBodyChunk req
                    if B.null bs then return n else count (n + B.length bs)
            n <- count (0 :: Int)
            sendResponse (responseBuilder ok200 [] (byteString (C8.pack (show n)))) []
        Just "/both" -> do
            -- Read the body on the side, so that the response does not
            -- wait for it.
            _ <-
                forkIO $
                    let d = getRequestBodyChunk req >>= \bs -> unless (B.null bs) d
                     in d
            sendResponse responseBoth []
        _ -> sendResponse responseHello []
    Just "HEAD" -> case requestPath req of
        -- HEADERS, then an empty DATA frame with END_STREAM.
        Just "/data" -> sendResponse (responseBuilder ok200 bigLength mempty) []
        -- HEADERS with END_STREAM.
        _ -> sendResponse (responseNoBody ok200 bigLength) []
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

-- | The stream error a client raises for a malformed response, as
-- 'sendRequest' hands it on.
malformedResponse :: C.HTTP2Error -> Bool
malformedResponse (C.StreamErrorIsSent C.ProtocolError _ _) = True
malformedResponse (C.BadThingHappen se) =
    maybe False malformedResponse $ E.fromException se
malformedResponse _ = False

-- | The content-length of content that is not there.
bigLength :: ResponseHeaders
bigLength = [("content-length", "1234")]

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

earlyHints103 :: Status
earlyHints103 = mkStatus 103 "Early Hints"

responsePushBig :: Response
responsePushBig = responseBuilder ok200 [] $ byteString $ C8.replicate 20000 'p'

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

responseEndless :: Response
responseEndless = responseStreaming ok200 [] body
  where
    body :: (Builder -> IO ()) -> IO () -> IO ()
    body write flush = forever (write (byteString chunk) *> flush) `E.catch` quiet
    chunk = C8.replicate 1024 'x'
    quiet :: E.SomeException -> IO ()
    quiet _ = return ()

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

-- | What the server says in its GOAWAY, if it sends one before closing.
awaitGoAway :: Socket -> IO (Maybe (ErrorCode, ByteString))
awaitGoAway s = do
    mf <- recvFrame s
    case mf of
        Nothing -> return Nothing
        Just (FrameGoAway, fh, p)
            | Right (GoAwayFrame _ err msg) <- decodeGoAwayFrame fh p ->
                return $ Just (err, msg)
        Just _ -> awaitGoAway s

-- | A SETTINGS_INITIAL_WINDOW_SIZE that takes an open stream's window past
-- 2^31-1.  What the server answers with: whether it reset the stream, and
-- the error in its GOAWAY.
settingsOverflow :: IO (Bool, Maybe ErrorCode)
settingsOverflow = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let sid = 1
        -- No END_STREAM: the stream stays open, waiting for the body.
        einfoH = EncodeInfo (setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/echo")
                , (":method", "POST")
                ]
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    -- The stream's window is now the largest there is ...
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags sid Nothing) $
            WindowUpdateFrame (maxWindowSize - defaultWindowSize)
    -- ... and one more octet of initial window takes it over.
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            SettingsFrame [(SettingsInitialWindowSize, defaultWindowSize + 1)]
    answer s False
  where
    answer s reset = do
        mf <- recvFrame s
        case mf of
            Nothing -> return (reset, Nothing)
            Just (FrameRSTStream, _, _) -> answer s True
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return (reset, Just err)
            Just _ -> answer s reset

-- | A request whose body ends with an empty trailer block.  What the
-- server answers it with.
emptyTrailers :: IO (Maybe String)
emptyTrailers = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let sid = 1
        einfoH = EncodeInfo (setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/count")
                , (":method", "POST")
                ]
        einfoT = EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    sendAll s $ encodeFrame (EncodeInfo defaultFlags sid Nothing) $ DataFrame "body"
    sendAll s $ encodeFrame einfoT $ HeadersFrame Nothing ""
    answer s sid
  where
    answer s sid = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameHeaders, fh, _)
                | streamId fh == sid -> return $ Just "HEADERS"
            Just (FrameRSTStream, fh, p)
                | streamId fh == sid
                , Right (RSTStreamFrame err) <- decodeRSTStreamFrame fh p ->
                    return $ Just $ "RST_STREAM " ++ show err
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err
            Just _ -> answer s sid

-- | A request with a content-length of 4, whose body comes in padded DATA
-- frames: "bo", "dy", and an empty one with END_STREAM.  What the server
-- answers it with: the body of its response, which is the number of octets
-- of body that arrived.
paddedBody :: IO (Maybe String)
paddedBody = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let sid = 1
        einfoH = EncodeInfo (setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/count")
                , (":method", "POST")
                , ("content-length", "4")
                ]
        padding = C8.replicate 10 '\0'
        einfoD = EncodeInfo defaultFlags sid (Just padding)
        einfoE = EncodeInfo (setEndStream defaultFlags) sid (Just padding)
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    sendAll s $ encodeFrame einfoD $ DataFrame "bo"
    sendAll s $ encodeFrame einfoD $ DataFrame "dy"
    sendAll s $ encodeFrame einfoE $ DataFrame ""
    answer s sid
  where
    answer s sid = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameData, fh, p)
                | streamId fh == sid -> return $ Just $ "DATA " ++ C8.unpack p
            Just (FrameRSTStream, fh, p)
                | streamId fh == sid
                , Right (RSTStreamFrame err) <- decodeRSTStreamFrame fh p ->
                    return $ Just $ "RST_STREAM " ++ show err
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err
            Just _ -> answer s sid

-- | A request whose body is 2000 DATA frames of one octet each, padded to
-- 257 octets of payload, more than the stream's window in all.  Sent
-- without waiting for WINDOW_UPDATE: every octet of padding has to have
-- been given back by the time the next frame is checked.  What the server
-- answers it with: the number of octets of body that arrived.
paddingWindow :: IO (Maybe String)
paddingWindow = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let sid = 1
        einfoH = EncodeInfo (setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/count")
                , (":method", "POST")
                ]
        padding = C8.replicate 255 '\0'
        einfoD = EncodeInfo defaultFlags sid (Just padding)
        einfoE = EncodeInfo (setEndStream defaultFlags) sid Nothing
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    replicateM_ 2000 $ sendAll s $ encodeFrame einfoD $ DataFrame "x"
    sendAll s $ encodeFrame einfoE $ DataFrame ""
    answer s sid
  where
    answer s sid = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameData, fh, p)
                | streamId fh == sid -> return $ Just $ "DATA " ++ C8.unpack p
            Just (FrameRSTStream, fh, p)
                | streamId fh == sid
                , Right (RSTStreamFrame err) <- decodeRSTStreamFrame fh p ->
                    return $ Just $ "RST_STREAM " ++ show err
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err msg) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err ++ " " ++ C8.unpack msg
            Just _ -> answer s sid

-- | 16384 octets of DATA on a stream we have half-closed, then a request
-- with a body of 16384 octets.  What the server gives back to the
-- connection window before answering the request, and the answer: the
-- number of octets of body that arrived.
refusedData :: IO (Maybe (Int, String))
refusedData = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let request sid method path flags =
            encodeFrame (EncodeInfo (flags $ setEndHeader defaultFlags) sid Nothing) $
                HeadersFrame Nothing $
                    hpackEncode
                        [ (":scheme", "http")
                        , (":authority", "127.0.0.1")
                        , (":path", path)
                        , (":method", method)
                        ]
        chunk = C8.replicate 16384 'x'
    -- A response that goes on for ever keeps stream 1 in the table.
    sendAll s $ request 1 "GET" "/stream" setEndStream
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 1 Nothing) $ DataFrame chunk
    sendAll s $ request 3 "POST" "/count" id
    sendAll s $
        encodeFrame (EncodeInfo (setEndStream defaultFlags) 3 Nothing) $
            DataFrame chunk
    answer s 0
  where
    answer s n = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameWindowUpdate, fh, p)
                | streamId fh == 0
                , Right (WindowUpdateFrame w) <- decodeWindowUpdateFrame fh p ->
                    answer s (n + w)
            Just (FrameData, fh, p)
                | streamId fh == 3 -> return $ Just (n, C8.unpack p)
            Just (FrameGoAway, _, _) -> return Nothing
            Just _ -> answer s n

-- | A request for /push, which comes with a push, from a peer that has
-- announced room for no streams of the server's.  What the server answers
-- it with first.
pushNoRoom :: IO (Maybe String)
pushNoRoom = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            SettingsFrame [(SettingsMaxConcurrentStreams, 0)]
    -- The server takes our SETTINGS on board once it has acknowledged them,
    -- and before it goes on to what comes next: the answer to a PING sent
    -- after them.
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            PingFrame "12345678"
    awaitPingAck s
    let sid = 1
        einfoH = EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/push")
                , (":method", "GET")
                ]
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    answer s sid
  where
    awaitPingAck s = do
        mf <- recvFrame s
        case mf of
            Just (FramePing, fh, _) | testAck (flags fh) -> return ()
            Just _ -> awaitPingAck s
            Nothing -> return ()
    answer s sid = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameHeaders, fh, _)
                | streamId fh == sid -> return $ Just "HEADERS"
            Just (FramePushPromise, _, _) -> return $ Just "PUSH_PROMISE"
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err
            Just _ -> answer s sid

-- | Two requests that are answered for ever, then SETTINGS that are a
-- connection error.  The last stream identifier and the error of the
-- server's GOAWAY.
goAwayLastStream :: IO (Maybe (StreamId, ErrorCode))
goAwayLastStream = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    forM_ [1, 3] $ \sid ->
        sendAll s $
            encodeFrame (EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing) $
                HeadersFrame Nothing $
                    hpackEncode
                        [ (":scheme", "http")
                        , (":authority", "127.0.0.1")
                        , (":path", "/endless")
                        , (":method", "GET")
                        ]
    -- SETTINGS_ENABLE_PUSH can only be 0 or 1.
    sendAll s $
        encodeFrame (EncodeInfo defaultFlags 0 Nothing) $
            SettingsFrame [(SettingsEnablePush, 2)]
    answer s
  where
    answer s = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame sid err _) <- decodeGoAwayFrame fh p ->
                    return $ Just (sid, err)
            Just _ -> answer s

-- | A request with an upper-case field name on stream 1, then a good one
-- on stream 3.  What the server sends on them, up to the answer on 3.
malformedRequest :: IO [String]
malformedRequest = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let request sid extra =
            encodeFrame (EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing) $
                HeadersFrame Nothing $
                    hpackEncode $
                        [ (":scheme", "http")
                        , (":authority", "127.0.0.1")
                        , (":path", "/")
                        , (":method", "GET")
                        ]
                            ++ extra
    sendAll s $ request 1 [("X-Upper", "1")]
    sendAll s $ request 3 []
    collect s
  where
    collect s = do
        mf <- recvFrame s
        case mf of
            Nothing -> return []
            Just (FrameHeaders, fh, _)
                | streamId fh == 3 -> return ["HEADERS 3"]
            Just (FrameRSTStream, fh, p)
                | Right (RSTStreamFrame err) <- decodeRSTStreamFrame fh p ->
                    (("RST_STREAM " ++ show (streamId fh) ++ " " ++ show err) :) <$> collect s
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return ["GOAWAY " ++ show err]
            Just _ -> collect s

-- | /endless until the server has used up the connection window, which we
-- do not open, then a request whose answer has no body: what the server
-- sends for it.  Then the windows opened a little: whether the body held
-- back goes on.
shutWindow :: IO (Maybe String)
shutWindow = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    let request sid path =
            encodeFrame (EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing) $
                HeadersFrame Nothing $
                    hpackEncode
                        [ (":scheme", "http")
                        , (":authority", "127.0.0.1")
                        , (":path", path)
                        , (":method", "GET")
                        ]
    sendAll s $ request 1 "/endless"
    used <- untilShut s 0
    if not used
        then return Nothing
        else do
            sendAll s $ request 3 "/not-modified"
            ma <- answer s
            case ma of
                Just "HEADERS 3" -> do
                    forM_ [0, 1] $ \sid ->
                        sendAll s $
                            encodeFrame (EncodeInfo defaultFlags sid Nothing) $
                                WindowUpdateFrame 1000
                    fmap ("HEADERS 3, then " ++) <$> resumed s
                _ -> return ma
  where
    resumed s = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameData, fh, _)
                | streamId fh == 1 -> return $ Just "DATA 1"
            Just _ -> resumed s
    untilShut s n
        | n >= defaultWindowSize = return True
        | otherwise = do
            mf <- recvFrame s
            case mf of
                Nothing -> return False
                Just (FrameData, fh, _) -> untilShut s (n + payloadLength fh)
                Just _ -> untilShut s n
    answer s = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameHeaders, fh, _)
                | streamId fh == 3 -> return $ Just "HEADERS 3"
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err
            Just _ -> answer s

-- | PRIORITY frames for 100 streams that are never opened, then a request.
-- What the server answers the request with.
idlePriority :: IO (Maybe String)
idlePriority = runTCPClient host port $ \s -> do
    sendAll s connectionPreface
    sendAll s $ encodeFrame (EncodeInfo defaultFlags 0 Nothing) $ SettingsFrame []
    forM_ [3, 5 .. 201] $ \sid ->
        sendAll s $
            encodeFrame (EncodeInfo defaultFlags sid Nothing) $
                PriorityFrame $
                    Priority False 0 16
    let sid = 203
        einfoH = EncodeInfo (setEndStream $ setEndHeader defaultFlags) sid Nothing
        hdr =
            hpackEncode
                [ (":scheme", "http")
                , (":authority", "127.0.0.1")
                , (":path", "/")
                , (":method", "GET")
                ]
    sendAll s $ encodeFrame einfoH $ HeadersFrame Nothing hdr
    answer s sid
  where
    answer s sid = do
        mf <- recvFrame s
        case mf of
            Nothing -> return Nothing
            Just (FrameHeaders, fh, _)
                | streamId fh == sid -> return $ Just "HEADERS"
            Just (FrameRSTStream, fh, p)
                | streamId fh == sid
                , Right (RSTStreamFrame err) <- decodeRSTStreamFrame fh p ->
                    return $ Just $ "RST_STREAM " ++ show err
            Just (FrameGoAway, fh, p)
                | Right (GoAwayFrame _ err _) <- decodeGoAwayFrame fh p ->
                    return $ Just $ "GOAWAY " ++ show err
            Just _ -> answer s sid

-- | Exactly so many octets off a raw connection, or fewer once it is closed.
recvAll :: Socket -> Int -> IO ByteString
recvAll s n0 = go n0 []
  where
    go 0 acc = return $ B.concat $ reverse acc
    go k acc = do
        bs <- recv s k
        if B.null bs
            then return $ B.concat $ reverse acc
            else go (k - B.length bs) (bs : acc)

-- | One frame off a raw connection, or 'Nothing' once it is closed.
recvFrame :: Socket -> IO (Maybe (FrameType, FrameHeader, ByteString))
recvFrame s = do
    mh <- recvExactly frameHeaderLength
    case mh of
        Nothing -> return Nothing
        Just h -> do
            let (ftyp, fh) = decodeFrameHeader h
            fmap (\p -> (ftyp, fh, p)) <$> recvExactly (payloadLength fh)
  where
    recvExactly n = go n []
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

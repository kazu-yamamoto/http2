{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Receiver (
    frameReceiver,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import Data.IORef
import UnliftIO.Concurrent
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports hiding (delete, insert)
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Arch.Config
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.HPACK
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Rate
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Arch.Window
import Network.HTTP2.Frame

----------------------------------------------------------------

continuationLimit :: Int
continuationLimit = 10

headerFragmentLimit :: Int
headerFragmentLimit = 51200 -- 50K

pingRateLimit :: Int
pingRateLimit = 4

settingsRateLimit :: Int
settingsRateLimit = 4

emptyFrameRateLimit :: Int
emptyFrameRateLimit = 4

rstRateLimit :: Int
rstRateLimit = 4

----------------------------------------------------------------

frameReceiver :: Context -> Config -> IO ()
frameReceiver ctx@Context{..} conf@Config{..} = loop 0 `E.catch` sendGoaway
  where
    loop :: Int -> IO ()
    loop n
        | n == 6 = do
            yield
            loop 0
        | otherwise = do
            hd <- confReadN frameHeaderLength
            if BS.null hd
                then enqueueControl controlQ $ CFinish ConnectionIsClosed
                else do
                    processFrame ctx conf $ decodeFrameHeader hd
                    loop (n + 1)

    sendGoaway se
        | Just e@ConnectionIsClosed <- E.fromException se =
            enqueueControl controlQ $ CFinish e
        | Just e@(ConnectionErrorIsReceived _ _ _) <- E.fromException se =
            enqueueControl controlQ $ CFinish e
        | Just e@(ConnectionErrorIsSent err sid msg) <- E.fromException se = do
            let frame = goawayFrame sid err $ Short.fromShort msg
            enqueueControl controlQ $ CFrames Nothing [frame]
            enqueueControl controlQ $ CFinish e
        | Just e@(StreamErrorIsSent err sid msg) <- E.fromException se = do
            let frame = resetFrame err sid
            enqueueControl controlQ $ CFrames Nothing [frame]
            let frame' = goawayFrame sid err $ Short.fromShort msg
            enqueueControl controlQ $ CFrames Nothing [frame']
            enqueueControl controlQ $ CFinish e
        | Just e@(StreamErrorIsReceived err sid) <- E.fromException se = do
            let frame = goawayFrame sid err "treat a stream error as a connection error"
            enqueueControl controlQ $ CFrames Nothing [frame]
            enqueueControl controlQ $ CFinish e
        -- this never happens
        | Just e@(BadThingHappen _) <- E.fromException se =
            enqueueControl controlQ $ CFinish e
        | otherwise =
            enqueueControl controlQ $ CFinish $ BadThingHappen se

----------------------------------------------------------------

processFrame :: Context -> Config -> (FrameType, FrameHeader) -> IO ()
processFrame ctx _conf (fid, FrameHeader{streamId})
    | isServer ctx
        && isServerInitiated streamId
        && (fid `notElem` [FramePriority, FrameRSTStream, FrameWindowUpdate]) =
        E.throwIO $
            ConnectionErrorIsSent ProtocolError streamId "stream id should be odd"
processFrame Context{..} Config{..} (ftyp, FrameHeader{payloadLength, streamId})
    | ftyp > maxFrameType = do
        mx <- readIORef continued
        case mx of
            Nothing -> do
                -- ignoring unknown frame
                void $ confReadN payloadLength
            Just _ -> E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "unknown frame"
processFrame ctx Config{..} (FramePushPromise, header@FrameHeader{payloadLength, streamId})
    | isServer ctx =
        E.throwIO $
            ConnectionErrorIsSent ProtocolError streamId "push promise is not allowed"
    | otherwise = do
        pl <- confReadN payloadLength
        PushPromiseFrame sid frag <- guardIt $ decodePushPromiseFrame header pl
        unless (isServerInitiated sid) $
            E.throwIO $
                ConnectionErrorIsSent ProtocolError streamId "wrong sid for push promise"
        when (frag == "") $
            E.throwIO $
                ConnectionErrorIsSent
                    ProtocolError
                    streamId
                    "wrong header fragment for push promise"
        (_, vt) <- hpackDecodeHeader frag streamId ctx
        let ClientInfo{..} = toClientInfo $ roleInfo ctx
        when
            ( getHeaderValue tokenAuthority vt == Just authority
                && getHeaderValue tokenScheme vt == Just scheme
            )
            $ do
                let mmethod = getHeaderValue tokenMethod vt
                    mpath = getHeaderValue tokenPath vt
                case (mmethod, mpath) of
                    (Just method, Just path) -> do
                        strm <- openStream ctx sid FramePushPromise
                        insertCache method path strm $ roleInfo ctx
                    _ -> return ()
processFrame ctx@Context{..} conf typhdr@(ftyp, header) = do
    -- My SETTINGS_MAX_FRAME_SIZE
    -- My SETTINGS_ENABLE_PUSH
    settings <- readIORef mySettings
    case checkFrameHeader settings typhdr of
        Left (FrameDecodeError ec sid msg) -> E.throwIO $ ConnectionErrorIsSent ec sid msg
        Right _ -> controlOrStream ctx conf ftyp header

----------------------------------------------------------------

controlOrStream :: Context -> Config -> FrameType -> FrameHeader -> IO ()
controlOrStream ctx@Context{..} conf@Config{..} ftyp header@FrameHeader{streamId, payloadLength}
    | isControl streamId = do
        pl <- confReadN payloadLength
        control ftyp header pl ctx conf
    | otherwise = do
        checkContinued
        mstrm <- getStream ctx ftyp streamId
        pl <- confReadN payloadLength
        case mstrm of
            Just strm -> do
                state0 <- readStreamState strm
                state <- stream ftyp header pl ctx state0 strm
                resetContinued
                set <- processState state ctx strm streamId
                when set setContinued
            Nothing
                | ftyp == FramePriority -> do
                    -- for h2spec only
                    PriorityFrame newpri <- guardIt $ decodePriorityFrame header pl
                    checkPriority newpri streamId
                | otherwise -> return ()
  where
    setContinued = writeIORef continued $ Just streamId
    resetContinued = writeIORef continued Nothing
    checkContinued = do
        mx <- readIORef continued
        case mx of
            Nothing -> return ()
            Just sid
                | sid == streamId && ftyp == FrameContinuation -> return ()
                | otherwise ->
                    E.throwIO $
                        ConnectionErrorIsSent ProtocolError streamId "continuation frame must follow"

----------------------------------------------------------------

processState :: StreamState -> Context -> Stream -> StreamId -> IO Bool
-- Transition (process1)
processState (Open _ (NoBody tbl@(_, reqvt))) ctx@Context{..} strm@Stream{streamInput} streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    when (just mcl (/= (0 :: Int))) $
        E.throwIO $
            StreamErrorIsSent
                ProtocolError
                streamId
                "no body but content-length is not zero"
    halfClosedRemote ctx strm
    tlr <- newIORef Nothing
    let inpObj = InpObj tbl (Just 0) (return "") tlr
    if isServer ctx
        then do
            let si = toServerInfo roleInfo
            atomically $ writeTQueue (inputQ si) $ Input strm inpObj
        else putMVar streamInput inpObj
    return False

-- Transition (process2)
processState (Open hcl (HasBody tbl@(_, reqvt))) ctx@Context{..} strm@Stream{streamInput} _streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    bodyLength <- newIORef 0
    tlr <- newIORef Nothing
    q <- newTQueueIO
    setStreamState ctx strm $ Open hcl (Body q mcl bodyLength tlr)
    incref <- newIORef 0
    bodySource <- mkSource q $ informWindowUpdate ctx strm incref
    let inpObj = InpObj tbl mcl (readSource bodySource) tlr
    if isServer ctx
        then do
            let si = toServerInfo roleInfo
            atomically $ writeTQueue (inputQ si) $ Input strm inpObj
        else putMVar streamInput inpObj
    return False

-- Transition (process3)
processState s@(Open _ Continued{}) ctx strm _streamId = do
    setStreamState ctx strm s
    return True

-- Transition (process4)
processState HalfClosedRemote ctx strm _streamId = do
    halfClosedRemote ctx strm
    return False

-- Transition (process5)
processState (Closed cc) ctx strm _streamId = do
    closed ctx strm cc
    return False

-- Transition (process6)
processState s ctx strm _streamId = do
    -- Idle, Open Body, Closed
    setStreamState ctx strm s
    return False

----------------------------------------------------------------

getStream :: Context -> FrameType -> StreamId -> IO (Maybe Stream)
getStream ctx@Context{..} ftyp streamId =
    search streamTable streamId >>= getStream' ctx ftyp streamId

getStream'
    :: Context -> FrameType -> StreamId -> Maybe Stream -> IO (Maybe Stream)
getStream' ctx ftyp streamId js@(Just strm0) = do
    when (ftyp == FrameHeaders) $ do
        st <- readStreamState strm0
        when (isHalfClosedRemote st) $
            E.throwIO $
                ConnectionErrorIsSent
                    StreamClosed
                    streamId
                    "header must not be sent to half or fully closed stream"
        -- Priority made an idle stream
        when (isIdle st) $ opened ctx strm0
    return js
getStream' ctx@Context{..} ftyp streamId Nothing
    | isServerInitiated streamId = return Nothing
    | isServer ctx = do
        csid <- getPeerStreamID ctx
        if streamId <= csid -- consider the stream closed
            then
                if ftyp `elem` [FrameWindowUpdate, FrameRSTStream, FramePriority]
                    then return Nothing -- will be ignored
                    else
                        E.throwIO $
                            ConnectionErrorIsSent
                                ProtocolError
                                streamId
                                "stream identifier must not decrease"
            else do
                -- consider the stream idle
                when (ftyp `notElem` [FrameHeaders, FramePriority]) $ do
                    let errmsg =
                            Short.toShort
                                ( "this frame is not allowed in an idle stream: "
                                    `BS.append` (C8.pack (show ftyp))
                                )
                    E.throwIO $ ConnectionErrorIsSent ProtocolError streamId errmsg
                when (ftyp == FrameHeaders) $ do
                    setPeerStreamID ctx streamId
                    cnt <- readIORef concurrency
                    -- Checking the limitation of concurrency
                    -- My SETTINGS_MAX_CONCURRENT_STREAMS
                    mMaxConc <- maxConcurrentStreams <$> readIORef mySettings
                    case mMaxConc of
                        Nothing -> return ()
                        Just maxConc ->
                            when (cnt >= maxConc) $
                                E.throwIO $
                                    StreamErrorIsSent RefusedStream streamId "exceeds max concurrent"
                Just <$> openStream ctx streamId ftyp
    | otherwise = undefined -- never reach

----------------------------------------------------------------

type Payload = ByteString

control :: FrameType -> FrameHeader -> Payload -> Context -> Config -> IO ()
control FrameSettings header@FrameHeader{flags, streamId} bs ctx@Context{myFirstSettings, myPendingAlist, mySettings, controlQ, settingsRate} conf = do
    SettingsFrame peerAlist <- guardIt $ decodeSettingsFrame header bs
    traverse_ E.throwIO $ checkSettingsList peerAlist
    if testAck flags
        then do
            when (peerAlist /= []) $
                E.throwIO $
                    ConnectionErrorIsSent FrameSizeError streamId "ack settings has a body"
            mAlist <- readIORef myPendingAlist
            case mAlist of
                Nothing -> return () -- fixme
                Just myAlist -> do
                    modifyIORef' mySettings $ \old -> updateSettings old myAlist
                    writeIORef myPendingAlist Nothing
        else do
            -- Settings Flood - CVE-2019-9515
            rate <- getRate settingsRate
            when (rate > settingsRateLimit) $
                E.throwIO $
                    ConnectionErrorIsSent ProtocolError streamId "too many settings"
            let ack = settingsFrame setAck []
            sent <- readIORef myFirstSettings
            if sent
                then do
                    let setframe = CFrames (Just peerAlist) [ack]
                    enqueueControl controlQ setframe
                else do
                    -- Server side only
                    frames <- updateMySettings conf ctx
                    let setframe = CFrames (Just peerAlist) (frames ++ [ack])
                    enqueueControl controlQ setframe
control FramePing FrameHeader{flags, streamId} bs Context{controlQ, pingRate} _ =
    unless (testAck flags) $ do
        -- Ping Flood - CVE-2019-9512
        rate <- getRate pingRate
        if rate > pingRateLimit
            then E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many ping"
            else do
                let frame = pingFrame bs
                enqueueControl controlQ $ CFrames Nothing [frame]
control FrameGoAway header bs _ _ = do
    GoAwayFrame sid err msg <- guardIt $ decodeGoAwayFrame header bs
    if err == NoError
        then E.throwIO ConnectionIsClosed
        else E.throwIO $ ConnectionErrorIsReceived err sid $ Short.toShort msg
control FrameWindowUpdate header bs ctx _ = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    increaseConnectionWindowSize ctx n
control _ _ _ _ _ =
    -- must not reach here
    return ()

----------------------------------------------------------------

{-# INLINE guardIt #-}
guardIt :: Either FrameDecodeError a -> IO a
guardIt x = case x of
    Left (FrameDecodeError ec sid msg) -> E.throwIO $ ConnectionErrorIsSent ec sid msg
    Right frame -> return frame

{-# INLINE checkPriority #-}
checkPriority :: Priority -> StreamId -> IO ()
checkPriority p me
    | dep == me =
        E.throwIO $ StreamErrorIsSent ProtocolError me "priority depends on itself"
    | otherwise = return ()
  where
    dep = streamDependency p

stream
    :: FrameType
    -> FrameHeader
    -> ByteString
    -> Context
    -> StreamState
    -> Stream
    -> IO StreamState
-- Transition (stream1)
stream FrameHeaders header@FrameHeader{flags, streamId} bs ctx s@(Open hcl JustOpened) Stream{streamNumber} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
        endOfHeader = testEndHeader flags
    if frag == "" && not endOfStream && not endOfHeader
        then do
            -- Empty Frame Flooding - CVE-2019-9518
            rate <- getRate $ emptyFrameRate ctx
            if rate > emptyFrameRateLimit
                then
                    E.throwIO $
                        ConnectionErrorIsSent ProtocolError streamId "too many empty headers"
                else return s
        else do
            case mp of
                Nothing -> return ()
                Just p -> checkPriority p streamNumber
            if endOfHeader
                then do
                    tbl <- hpackDecodeHeader frag streamId ctx
                    return $
                        if endOfStream
                            then -- turned into HalfClosedRemote in processState
                                Open hcl (NoBody tbl)
                            else Open hcl (HasBody tbl)
                else do
                    let siz = BS.length frag
                    return $ Open hcl $ Continued [frag] siz 1 endOfStream

-- Transition (stream2)
stream FrameHeaders header@FrameHeader{flags, streamId} bs ctx (Open _ (Body q _ _ tlr)) _ = do
    HeadersFrame _ frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
    -- checking frag == "" is not necessary
    if endOfStream
        then do
            tbl <- hpackDecodeTrailer frag streamId ctx
            writeIORef tlr (Just tbl)
            atomically $ writeTQueue q $ Right ""
            return HalfClosedRemote
        else -- we don't support continuation here.

            E.throwIO $
                ConnectionErrorIsSent
                    ProtocolError
                    streamId
                    "continuation in trailer is not supported"

-- Transition (stream4)
stream
    FrameData
    header@FrameHeader{flags, payloadLength, streamId}
    bs
    Context{emptyFrameRate}
    s@(Open _ (Body q mcl bodyLength _))
    _ = do
        DataFrame body <- guardIt $ decodeDataFrame header bs
        len0 <- readIORef bodyLength
        let len = len0 + payloadLength
            endOfStream = testEndStream flags
        -- Empty Frame Flooding - CVE-2019-9518
        if body == ""
            then unless endOfStream $ do
                rate <- getRate emptyFrameRate
                when (rate > emptyFrameRateLimit) $ do
                    E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many empty data"
            else do
                writeIORef bodyLength len
                atomically $ writeTQueue q $ Right body
        if endOfStream
            then do
                case mcl of
                    Nothing -> return ()
                    Just cl ->
                        when (cl /= len) $
                            E.throwIO $
                                StreamErrorIsSent
                                    ProtocolError
                                    streamId
                                    "actual body length is not the same as content-length"
                -- no trailers
                atomically $ writeTQueue q $ Right ""
                return HalfClosedRemote
            else return s

-- Transition (stream5)
stream FrameContinuation FrameHeader{flags, streamId} frag ctx s@(Open hcl (Continued rfrags siz n endOfStream)) _ = do
    let endOfHeader = testEndHeader flags
    if frag == "" && not endOfHeader
        then do
            -- Empty Frame Flooding - CVE-2019-9518
            rate <- getRate $ emptyFrameRate ctx
            if rate > emptyFrameRateLimit
                then
                    E.throwIO $
                        ConnectionErrorIsSent ProtocolError streamId "too many empty continuation"
                else return s
        else do
            let rfrags' = frag : rfrags
                siz' = siz + BS.length frag
                n' = n + 1
            when (siz' > headerFragmentLimit) $
                E.throwIO $
                    ConnectionErrorIsSent EnhanceYourCalm streamId "Header is too big"
            when (n' > continuationLimit) $
                E.throwIO $
                    ConnectionErrorIsSent EnhanceYourCalm streamId "Header is too fragmented"
            if endOfHeader
                then do
                    let hdrblk = BS.concat $ reverse rfrags'
                    tbl <- hpackDecodeHeader hdrblk streamId ctx
                    return $
                        if endOfStream
                            then -- turned into HalfClosedRemote in processState
                                Open hcl (NoBody tbl)
                            else Open hcl (HasBody tbl)
                else return $ Open hcl $ Continued rfrags' siz' n' endOfStream

-- (No state transition)
stream FrameWindowUpdate header bs _ s strm = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    increaseStreamWindowSize strm n
    return s

-- Transition (stream6)
stream FrameRSTStream header@FrameHeader{streamId} bs ctx s strm = do
    rate <- getRate $ rstRate ctx
    when (rate > rstRateLimit) $
        E.throwIO $
            ConnectionErrorIsSent ProtocolError streamId "too many rst_stream"
    RSTStreamFrame err <- guardIt $ decodeRSTStreamFrame header bs
    let cc = Reset err

    -- The spec mandates (section 8.1):
    --
    -- > When this is true, a server MAY request that the client abort
    -- > transmission of a request without error by sending a RST_STREAM with an
    -- > error code of NO_ERROR after sending a complete response (i.e., a frame
    -- > with the END_STREAM flag).
    --
    -- We check the first part ("after sending a complete response") by checking
    -- the current stream state.
    case (s, err) of
        (HalfClosedRemote, NoError) ->
            return (Closed cc)
        _otherwise -> do
            closed ctx strm cc
            E.throwIO $ StreamErrorIsReceived err streamId

-- (No state transition)
stream FramePriority header bs _ s Stream{streamNumber} = do
    -- ignore
    -- Resource Loop - CVE-2019-9513
    PriorityFrame newpri <- guardIt $ decodePriorityFrame header bs
    checkPriority newpri streamNumber
    return s

-- this ordering is important
stream FrameContinuation FrameHeader{streamId} _ _ _ _ =
    E.throwIO $
        ConnectionErrorIsSent ProtocolError streamId "continue frame cannot come here"
stream _ FrameHeader{streamId} _ _ (Open _ Continued{}) _ =
    E.throwIO $
        ConnectionErrorIsSent
            ProtocolError
            streamId
            "an illegal frame follows header/continuation frames"
-- Ignore frames to streams we have just reset, per section 5.1.
stream _ _ _ _ st@(Closed (ResetByMe _)) _ = return st
stream FrameData FrameHeader{streamId} _ _ _ _ =
    E.throwIO $
        StreamErrorIsSent StreamClosed streamId $
            fromString ("illegal data frame for " ++ show streamId)
stream _ FrameHeader{streamId} _ _ _ _ =
    E.throwIO $
        StreamErrorIsSent ProtocolError streamId $
            fromString ("illegal frame for " ++ show streamId)

----------------------------------------------------------------

-- | Type for input streaming.
data Source
    = Source
        (Int -> IO ())
        (TQueue (Either E.SomeException ByteString))
        (IORef ByteString)
        (IORef Bool)

mkSource
    :: TQueue (Either E.SomeException ByteString) -> (Int -> IO ()) -> IO Source
mkSource q inform = Source inform q <$> newIORef "" <*> newIORef False

readSource :: Source -> IO ByteString
readSource (Source inform q refBS refEOF) = do
    eof <- readIORef refEOF
    if eof
        then return ""
        else do
            bs <- readBS
            let len = BS.length bs
            inform len
            return bs
  where
    readBS :: IO ByteString
    readBS = do
        bs0 <- readIORef refBS
        if bs0 == ""
            then do
                mBS <- atomically $ readTQueue q
                case mBS of
                    Left err -> do
                        writeIORef refEOF True
                        E.throwIO err
                    Right bs -> do
                        when (bs == "") $ writeIORef refEOF True
                        return bs
            else do
                writeIORef refBS ""
                return bs0

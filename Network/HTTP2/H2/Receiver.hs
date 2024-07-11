{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Receiver (
    frameReceiver,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import Network.Control
import Network.HTTP.Semantics
import UnliftIO.Concurrent
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports hiding (delete, insert)
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.EncodeFrame
import Network.HTTP2.H2.HPACK
import Network.HTTP2.H2.Manager
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Settings
import Network.HTTP2.H2.Stream
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

----------------------------------------------------------------

continuationLimit :: Int
continuationLimit = 10

headerFragmentLimit :: Int
headerFragmentLimit = 51200 -- 50K

----------------------------------------------------------------

frameReceiver :: Context -> Config -> IO ()
frameReceiver ctx@Context{..} conf@Config{..} = do
    labelMe "H2 receiver"
    loop `E.catch` sendGoaway
  where
    loop = do
        -- If 'confReadN' is timeouted, an exception is thrown
        -- to destroy the thread trees.
        hd <- confReadN frameHeaderLength
        if BS.null hd
            then enqueueControl controlQ $ CFinish ConnectionIsTimeout
            else do
                processFrame ctx conf $ decodeFrameHeader hd
                loop

    sendGoaway se
        | Just ConnectionIsClosed <- E.fromException se = do
            waitCounter0 threadManager
            enqueueControl controlQ $ CFinish ConnectionIsClosed
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
processFrame ctx _conf (FramePushPromise, FrameHeader{streamId})
    | isServer ctx =
        E.throwIO $
            ConnectionErrorIsSent ProtocolError streamId "push promise is not allowed"
processFrame Context{..} Config{..} (ftyp, FrameHeader{payloadLength, streamId})
    | ftyp > maxFrameType = do
        mx <- readIORef continued
        case mx of
            Nothing -> do
                -- ignoring unknown frame
                void $ confReadN payloadLength
            Just _ -> E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "unknown frame"
processFrame ctx@Context{..} conf typhdr@(ftyp, header) = do
    -- My SETTINGS_MAX_FRAME_SIZE
    -- My SETTINGS_ENABLE_PUSH
    case checkFrameHeader typhdr of
        Left (FrameDecodeError ec sid msg) -> E.throwIO $ ConnectionErrorIsSent ec sid msg
        Right _ -> do
            let Settings{maxFrameSize, enablePush} = mySettings
                sid = streamId header
            when (payloadLength header > maxFrameSize) $
                E.throwIO $
                    ConnectionErrorIsSent FrameSizeError sid "exceeds maximum frame size"
            when (not enablePush && ftyp == FramePushPromise) $
                E.throwIO $
                    ConnectionErrorIsSent ProtocolError sid "push not enabled"
            controlOrStream ctx conf ftyp header

----------------------------------------------------------------

controlOrStream :: Context -> Config -> FrameType -> FrameHeader -> IO ()
controlOrStream ctx@Context{..} Config{..} ftyp header@FrameHeader{streamId, payloadLength}
    | isControl streamId = do
        bs <- confReadN payloadLength
        control ftyp header bs ctx
    | ftyp == FramePushPromise = do
        bs <- confReadN payloadLength
        push header bs ctx
    | otherwise = do
        checkContinued
        mstrm <- getStream ctx ftyp streamId
        bs <- confReadN payloadLength
        case mstrm of
            Just strm -> do
                state0 <- readStreamState strm
                state <- stream ftyp header bs ctx state0 strm
                resetContinued
                set <- processState state ctx strm streamId
                when set setContinued
            Nothing
                | ftyp == FramePriority -> do
                    -- for h2spec only
                    PriorityFrame newpri <- guardIt $ decodePriorityFrame header bs
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
    let mcl = fst <$> (getFieldValue tokenContentLength reqvt >>= C8.readInt)
    when (just mcl (/= (0 :: Int))) $
        E.throwIO $
            StreamErrorIsSent
                ProtocolError
                streamId
                "no body but content-length is not zero"
    tlr <- newIORef Nothing
    let inpObj = InpObj tbl (Just 0) (return (mempty, True)) tlr
    if isServer ctx
        then do
            let ServerInfo{..} = toServerInfo roleInfo
            launch ctx strm inpObj
        else putMVar streamInput $ Right inpObj
    halfClosedRemote ctx strm
    return False

-- Transition (process2)
processState (Open hcl (HasBody tbl@(_, reqvt))) ctx@Context{..} strm@Stream{streamInput, streamRxQ} _streamId = do
    let mcl = fst <$> (getFieldValue tokenContentLength reqvt >>= C8.readInt)
    bodyLength <- newIORef 0
    tlr <- newIORef Nothing
    q <- newTQueueIO
    writeIORef streamRxQ $ Just q
    setStreamState ctx strm $ Open hcl (Body q mcl bodyLength tlr)
    -- FLOW CONTROL: WINDOW_UPDATE 0: recv: announcing my limit properly
    -- FLOW CONTROL: WINDOW_UPDATE: recv: announcing my limit properly
    bodySource <- mkSource q $ informWindowUpdate ctx strm
    let inpObj = InpObj tbl mcl (readSource bodySource) tlr
    if isServer ctx
        then do
            let ServerInfo{..} = toServerInfo roleInfo
            launch ctx strm inpObj
        else putMVar streamInput $ Right inpObj
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

{- FOURMOLU_DISABLE -}
getStream :: Context -> FrameType -> StreamId -> IO (Maybe Stream)
getStream ctx@Context{..} ftyp streamId
  | isEven    = lookupEven evenStreamTable streamId >>= getEvenStream ctx ftyp
  | otherwise = lookupOdd oddStreamTable  streamId >>= getOddStream  ctx ftyp streamId
  where
    isEven = isServerInitiated streamId
{- FOURMOLU_ENABLE -}

getEvenStream :: Context -> FrameType -> Maybe Stream -> IO (Maybe Stream)
getEvenStream ctx ftyp js@(Just strm) = do
    when (ftyp == FrameHeaders) $ do
        st <- readStreamState strm
        when (isReserved st) $ halfClosedLocal ctx strm Finished
    return js
getEvenStream _ _ Nothing = return Nothing

getOddStream
    :: Context -> FrameType -> StreamId -> Maybe Stream -> IO (Maybe Stream)
getOddStream ctx ftyp streamId js@(Just strm0) = do
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
getOddStream ctx ftyp streamId Nothing
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
                when (ftyp == FrameHeaders) $ setPeerStreamID ctx streamId
                -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: recv: rejecting if over my limit
                Just <$> openOddStreamCheck ctx streamId ftyp
    | otherwise =
        -- We received a frame from the server on an unknown stream
        -- (likely a previously created and then subsequently reset stream).
        -- We just drop it.
        return Nothing

----------------------------------------------------------------

type Payload = ByteString

control :: FrameType -> FrameHeader -> Payload -> Context -> IO ()
control FrameSettings header@FrameHeader{flags, streamId} bs Context{myFirstSettings, controlQ, settingsRate, mySettings, rxFlow} = do
    SettingsFrame peerAlist <- guardIt $ decodeSettingsFrame header bs
    traverse_ E.throwIO $ checkSettingsList peerAlist
    if testAck flags
        then do
            when (peerAlist /= []) $
                E.throwIO $
                    ConnectionErrorIsSent FrameSizeError streamId "ack settings has a body"
        else do
            -- Settings Flood - CVE-2019-9515
            rate <- getRate settingsRate
            when (rate > settingsRateLimit mySettings) $
                E.throwIO $
                    ConnectionErrorIsSent EnhanceYourCalm streamId "too many settings"
            let ack = settingsFrame setAck []
            sent <- readIORef myFirstSettings
            if sent
                then do
                    let setframe = CFrames (Just peerAlist) [ack]
                    enqueueControl controlQ setframe
                else do
                    -- Server side only
                    connRxWS <- rxfBufSize <$> readIORef rxFlow
                    let frames = makeNegotiationFrames mySettings connRxWS
                        setframe = CFrames (Just peerAlist) (frames ++ [ack])
                    writeIORef myFirstSettings True
                    enqueueControl controlQ setframe
control FramePing FrameHeader{flags, streamId} bs Context{mySettings, controlQ, pingRate} =
    unless (testAck flags) $ do
        rate <- getRate pingRate
        if rate > pingRateLimit mySettings
            then E.throwIO $ ConnectionErrorIsSent EnhanceYourCalm streamId "too many ping"
            else do
                let frame = pingFrame bs
                enqueueControl controlQ $ CFrames Nothing [frame]
control FrameGoAway header bs _ = do
    GoAwayFrame sid err msg <- guardIt $ decodeGoAwayFrame header bs
    if err == NoError
        then E.throwIO ConnectionIsClosed
        else E.throwIO $ ConnectionErrorIsReceived err sid $ Short.toShort msg
control FrameWindowUpdate header bs ctx = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    increaseConnectionWindowSize ctx n
control _ _ _ _ =
    -- must not reach here
    return ()

----------------------------------------------------------------

-- Called in client only
push :: FrameHeader -> ByteString -> Context -> IO ()
push header@FrameHeader{streamId} bs ctx = do
    PushPromiseFrame sid frag <- guardIt $ decodePushPromiseFrame header bs
    unless (isServerInitiated sid) $
        E.throwIO $
            ConnectionErrorIsSent
                ProtocolError
                streamId
                "push promise must specify an even stream identifier"
    when (frag == "") $
        E.throwIO $
            ConnectionErrorIsSent
                ProtocolError
                streamId
                "wrong header fragment for push promise"
    (_, vt) <- hpackDecodeHeader frag streamId ctx
    let ClientInfo{..} = toClientInfo $ roleInfo ctx
    when
        ( getFieldValue tokenAuthority vt == Just (UTF8.fromString authority)
            && getFieldValue tokenScheme vt == Just scheme
        )
        $ do
            let mmethod = getFieldValue tokenMethod vt
                mpath = getFieldValue tokenPath vt
            case (mmethod, mpath) of
                (Just method, Just path) ->
                    -- FLOW CONTROL: SETTINGS_MAX_CONCURRENT_STREAMS: recv: rejecting if over my limit
                    openEvenStreamCacheCheck ctx sid method path
                _ -> return ()

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
            if rate > emptyFrameRateLimit (mySettings ctx)
                then
                    E.throwIO $
                        ConnectionErrorIsSent EnhanceYourCalm streamId "too many empty headers"
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
            atomically $ writeTQueue q $ Right (mempty, True)
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
    Context{emptyFrameRate, rxFlow, mySettings}
    s@(Open _ (Body q mcl bodyLength _))
    Stream{..} = do
        DataFrame body <- guardIt $ decodeDataFrame header bs
        -- FLOW CONTROL: WINDOW_UPDATE 0: recv: rejecting if over my limit
        okc <- atomicModifyIORef' rxFlow $ checkRxLimit payloadLength
        unless okc $
            E.throwIO $
                ConnectionErrorIsSent
                    EnhanceYourCalm
                    streamId
                    "exceeds connection flow-control limit"
        -- FLOW CONTROL: WINDOW_UPDATE: recv: rejecting if over my limit
        oks <- atomicModifyIORef' streamRxFlow $ checkRxLimit payloadLength
        unless oks $
            E.throwIO $
                ConnectionErrorIsSent
                    EnhanceYourCalm
                    streamId
                    "exceeds stream flow-control limit"
        len0 <- readIORef bodyLength
        let len = len0 + payloadLength
            endOfStream = testEndStream flags
        -- Empty Frame Flooding - CVE-2019-9518
        if body == ""
            then unless endOfStream $ do
                rate <- getRate emptyFrameRate
                when (rate > emptyFrameRateLimit mySettings) $ do
                    E.throwIO $ ConnectionErrorIsSent EnhanceYourCalm streamId "too many empty data"
            else do
                writeIORef bodyLength len
                atomically $ writeTQueue q $ Right (body, endOfStream)
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
                atomically $ writeTQueue q $ Right (mempty, True)
                return HalfClosedRemote
            else return s

-- Transition (stream5)
stream FrameContinuation FrameHeader{flags, streamId} frag ctx s@(Open hcl (Continued rfrags siz n endOfStream)) _ = do
    let endOfHeader = testEndHeader flags
    if frag == "" && not endOfHeader
        then do
            -- Empty Frame Flooding - CVE-2019-9518
            rate <- getRate $ emptyFrameRate ctx
            if rate > emptyFrameRateLimit (mySettings ctx)
                then
                    E.throwIO $
                        ConnectionErrorIsSent EnhanceYourCalm streamId "too many empty continuation"
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
    -- Rapid Rest: CVE-2023-44487
    rate <- getRate $ rstRate ctx
    when (rate > rstRateLimit (mySettings ctx)) $
        E.throwIO $
            ConnectionErrorIsSent EnhanceYourCalm streamId "too many rst_stream"
    RSTStreamFrame err <- guardIt $ decodeRSTStreamFrame header bs
    let cc = Reset err
    closed ctx strm cc

    -- HTTP2 spec, section 5.1, "Stream States":
    --
    -- > A stream in the "open" state may be used by both peers to send frames
    -- > of any type. (..) From this state, either endpoint can send a frame
    -- > with an END_STREAM flag set, which causes the stream to transition into
    -- > one of the "half-closed" states.  An endpoint sending an END_STREAM
    -- > flag causes the stream state to become "half-closed (local)"; an
    -- > endpoint receiving an END_STREAM flag causes the stream state to become
    -- > "half-closed (remote)".
    --
    -- Crucially (for the specific case we're dealing with here), it continues:
    --
    -- > /Either endpoint/ can send a RST_STREAM frame from this state, causing
    -- > it to transition immediately to "closed".
    --
    -- (emphasis not in original). This justifies the two non-error cases,
    -- below. (Section 8.1 of the spec is also relevant, but it is less explicit
    -- about the /either endpoint/ part.)
    case (s, err) of
        (Open (Just _) _, NoError) ->
            -- HalfClosedLocal
            return (Closed cc)
        (HalfClosedRemote, NoError) ->
            return (Closed cc)
        _otherwise -> do
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
stream x FrameHeader{streamId} _ _ _ _ =
    E.throwIO $
        StreamErrorIsSent ProtocolError streamId $
            fromString ("illegal frame " ++ show x ++ " for " ++ show streamId)

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source (Int -> IO ()) RxQ (IORef ByteString) (IORef Bool)

mkSource :: RxQ -> (Int -> IO ()) -> IO Source
mkSource q inform = Source inform q <$> newIORef "" <*> newIORef False

readSource :: Source -> IO (ByteString, Bool)
readSource (Source inform q refBS refEOF) = do
    eof <- readIORef refEOF
    if eof
        then return (mempty, True)
        else do
            (bs, isEOF) <- readBS
            let len = BS.length bs
            inform len
            return (bs, isEOF)
  where
    readBS :: IO (ByteString, Bool)
    readBS = do
        bs0 <- readIORef refBS
        if bs0 == ""
            then do
                mBS <- atomically $ readTQueue q
                case mBS of
                    Left err -> do
                        writeIORef refEOF True
                        E.throwIO err
                    Right (bs, isEOF) -> do
                        writeIORef refEOF isEOF
                        return (bs, isEOF)
            else do
                writeIORef refBS ""
                return (bs0, False)

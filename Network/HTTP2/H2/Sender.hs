{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP2.H2.Sender (
    frameSender,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Foreign.Ptr (minusPtr, plusPtr)
import Network.ByteOrder
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.IO

import Imports
import Network.HPACK (setLimitForEncoding, toTokenHeaderTable)
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.EncodeFrame
import Network.HTTP2.H2.HPACK
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Settings
import Network.HTTP2.H2.Stream
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

----------------------------------------------------------------

data Switch
    = C Control
    | O Output
    | Flush

wrapException :: E.SomeException -> IO ()
wrapException se
    | Just GoAwayIsSent <- E.fromException se = return ()
    | Just ConnectionIsClosed <- E.fromException se = return ()
    | Just (e :: HTTP2Error) <- E.fromException se = E.throwIO e
    | otherwise = E.throwIO $ BadThingHappen se

-- Peer SETTINGS_INITIAL_WINDOW_SIZE
-- Adjusting initial window size for streams
updatePeerSettings :: Context -> SettingsList -> IO ()
updatePeerSettings Context{peerSettings, oddStreamTable, evenStreamTable} peerAlist = do
    oldws <- initialWindowSize <$> readIORef peerSettings
    modifyIORef' peerSettings $ \old -> fromSettingsList old peerAlist
    newws <- initialWindowSize <$> readIORef peerSettings
    -- FIXME: race condition
    -- 1) newOddStream reads old peerSettings and
    --    insert it to its stream table after adjusting.
    -- 2) newOddStream reads new peerSettings and
    --    insert it to its stream table before adjusting.
    let dif = newws - oldws
    when (dif /= 0) $ do
        getOddStreams oddStreamTable >>= updateAllStreamTxFlow dif
        getEvenStreams evenStreamTable >>= updateAllStreamTxFlow dif
  where
    updateAllStreamTxFlow :: WindowSize -> IntMap Stream -> IO ()
    updateAllStreamTxFlow siz strms =
        forM_ strms $ \strm -> increaseStreamWindowSize strm siz

frameSender :: Context -> Config -> IO ()
frameSender
    ctx@Context{outputQ, controlQ, encodeDynamicTable, outputBufferLimit, senderDone}
    Config{..} = do
        labelMe "H2 sender"
        (loop 0 `E.finally` setSenderDone) `E.catch` wrapException
      where
        ----------------------------------------------------------------
        loop :: Offset -> IO ()
        loop off = do
            x <- atomically $ dequeue off
            case x of
                C ctl -> flushN off >> control ctl >> loop 0
                O out -> outputAndSync out off >>= flushIfNecessary >>= loop
                Flush -> flushN off >> loop 0

        -- Flush the connection buffer to the socket, where the first 'n' bytes of
        -- the buffer are filled.
        flushN :: Offset -> IO ()
        flushN 0 = return ()
        flushN n = bufferIO confWriteBuffer n confSendAll

        flushIfNecessary :: Offset -> IO Offset
        flushIfNecessary off = do
            buflim <- readIORef outputBufferLimit
            if off <= buflim - 512
                then return off
                else do
                    flushN off
                    return 0

        dequeue :: Offset -> STM Switch
        dequeue off = do
            isEmptyC <- isEmptyTQueue controlQ
            if isEmptyC
                then do
                    -- FLOW CONTROL: WINDOW_UPDATE 0: send: respecting peer's limit
                    waitConnectionWindowSize ctx
                    isEmptyO <- isEmptyTQueue outputQ
                    if isEmptyO
                        then if off /= 0 then return Flush else retry
                        else O <$> readTQueue outputQ
                else C <$> readTQueue controlQ

        ----------------------------------------------------------------
        copyAll [] buf = return buf
        copyAll (x : xs) buf = copy buf x >>= copyAll xs

        -- called with off == 0
        control :: Control -> IO ()
        control (CFinish e) = E.throwIO e
        control (CFrames ms xs) = do
            buf <- copyAll xs confWriteBuffer
            let off = buf `minusPtr` confWriteBuffer
            flushN off
            case ms of
                Nothing -> return ()
                Just peerAlist -> do
                    -- Peer SETTINGS_INITIAL_WINDOW_SIZE
                    updatePeerSettings ctx peerAlist
                    -- Peer SETTINGS_MAX_FRAME_SIZE
                    case lookup SettingsMaxFrameSize peerAlist of
                        Nothing -> return ()
                        Just payloadLen -> do
                            let dlim = payloadLen + frameHeaderLength
                                buflim
                                    | confBufferSize >= dlim = dlim
                                    | otherwise = confBufferSize
                            writeIORef outputBufferLimit buflim
                    -- Peer SETTINGS_HEADER_TABLE_SIZE
                    case lookup SettingsTokenHeaderTableSize peerAlist of
                        Nothing -> return ()
                        Just siz -> setLimitForEncoding siz encodeDynamicTable

        ----------------------------------------------------------------
        -- INVARIANT
        --
        -- Both the stream window and the connection window are open.
        ----------------------------------------------------------------
        outputAndSync :: Output -> Offset -> IO Offset
        outputAndSync out@(Output strm otyp sync) off = E.handle (\e -> resetStream strm InternalError e >> return off) $ do
            state <- readStreamState strm
            if isHalfClosedLocal state
                then return off
                else case otyp of
                    OHeader hdr mnext tlrmkr -> do
                        (off', mout') <- outputHeader strm hdr mnext tlrmkr sync off
                        case mout' of
                            Nothing -> sync Done
                            Just out' -> sync $ Cont out'
                        return off'
                    _ -> do
                        sws <- getStreamWindowSize strm
                        cws <- getConnectionWindowSize ctx -- not 0
                        let lim = min cws sws
                        (off', mout') <- output out off lim
                        case mout' of
                            Nothing -> sync Done
                            Just out' -> sync $ Cont out'
                        return off'

        resetStream :: Stream -> ErrorCode -> E.SomeException -> IO ()
        resetStream strm err e = do
            closed ctx strm (ResetByMe e)
            let rst = resetFrame err $ streamNumber strm
            enqueueControl controlQ $ CFrames Nothing [rst]

        ----------------------------------------------------------------
        outputHeader
            :: Stream
            -> [Header]
            -> Maybe DynaNext
            -> TrailersMaker
            -> (Sync -> IO ())
            -> Offset
            -> IO (Offset, Maybe Output)
        outputHeader strm hdr mnext tlrmkr sync off0 = do
            -- Header frame and Continuation frame
            let sid = streamNumber strm
                endOfStream = isNothing mnext
            (ths, _) <- toTokenHeaderTable $ fixHeaders hdr
            off' <- headerContinue sid ths endOfStream off0
            -- halfClosedLocal calls closed which removes
            -- the stream from stream table.
            off <- flushIfNecessary off'
            case mnext of
                Nothing -> do
                    -- endOfStream
                    halfClosedLocal ctx strm Finished
                    return (off, Nothing)
                Just next -> do
                    let out' = Output strm (ONext next tlrmkr) sync
                    return (off, Just out')

        ----------------------------------------------------------------
        output :: Output -> Offset -> WindowSize -> IO (Offset, Maybe Output)
        output out@(Output strm (ONext curr tlrmkr) _) off0 lim = do
            -- Data frame payload
            buflim <- readIORef outputBufferLimit
            let payloadOff = off0 + frameHeaderLength
                datBuf = confWriteBuffer `plusPtr` payloadOff
                datBufSiz = buflim - payloadOff
            curr datBuf (min datBufSiz lim) >>= \next ->
                case next of
                    Next datPayloadLen reqflush mnext -> do
                        NextTrailersMaker tlrmkr' <- runTrailersMaker tlrmkr datBuf datPayloadLen
                        fillDataHeader
                            strm
                            off0
                            datPayloadLen
                            mnext
                            tlrmkr'
                            out
                            reqflush
                    CancelNext mErr -> do
                        -- Stream cancelled
                        --
                        -- At this point, the headers have already been sent.
                        -- Therefore, the stream cannot be in the 'Idle' state, so we
                        -- are justified in sending @RST_STREAM@.
                        --
                        -- By the invariant on the 'outputQ', there are no other
                        -- outputs for this stream already enqueued. Therefore, we can
                        -- safely cancel it knowing that we won't try and send any
                        -- more data frames on this stream.
                        case mErr of
                            Just err ->
                                resetStream strm InternalError err
                            Nothing ->
                                resetStream strm Cancel (E.toException CancelledStream)
                        return (off0, Nothing)
        output (Output strm (OPush ths pid) _) off0 _lim = do
            -- Creating a push promise header
            -- Frame id should be associated stream id from the client.
            let sid = streamNumber strm
            len <- pushPromise pid sid ths off0
            off <- flushIfNecessary $ off0 + frameHeaderLength + len
            return (off, Nothing)
        output _ _ _ = undefined -- never reached

        ----------------------------------------------------------------
        headerContinue :: StreamId -> TokenHeaderList -> Bool -> Offset -> IO Offset
        headerContinue sid ths0 endOfStream off0 = do
            buflim <- readIORef outputBufferLimit
            let offkv = off0 + frameHeaderLength
                bufkv = confWriteBuffer `plusPtr` offkv
                limkv = buflim - offkv
            (ths, kvlen) <- hpackEncodeHeader ctx bufkv limkv ths0
            if kvlen == 0
                then continue off0 ths FrameHeaders
                else do
                    let flag = getFlag ths
                        buf = confWriteBuffer `plusPtr` off0
                        off = offkv + kvlen
                    fillFrameHeader FrameHeaders kvlen sid flag buf
                    continue off ths FrameContinuation
          where
            eos = if endOfStream then setEndStream else id
            getFlag [] = eos $ setEndHeader defaultFlags
            getFlag _ = eos $ defaultFlags

            continue :: Offset -> TokenHeaderList -> FrameType -> IO Offset
            continue off [] _ = return off
            continue off ths ft = do
                flushN off
                -- Now off is 0
                buflim <- readIORef outputBufferLimit
                let bufHeaderPayload = confWriteBuffer `plusPtr` frameHeaderLength

                    headerPayloadLim = buflim - frameHeaderLength
                (ths', kvlen') <-
                    hpackEncodeHeaderLoop ctx bufHeaderPayload headerPayloadLim ths
                when (ths == ths') $
                    E.throwIO $
                        ConnectionErrorIsSent CompressionError sid "cannot compress the header"
                let flag = getFlag ths'
                    off' = frameHeaderLength + kvlen'
                fillFrameHeader ft kvlen' sid flag confWriteBuffer
                continue off' ths' FrameContinuation

        ----------------------------------------------------------------
        fillDataHeader
            :: Stream
            -> Offset
            -> Int
            -> Maybe DynaNext
            -> (Maybe ByteString -> IO NextTrailersMaker)
            -> Output
            -> Bool
            -> IO (Offset, Maybe Output)
        fillDataHeader
            strm@Stream{streamNumber}
            off
            datPayloadLen
            Nothing
            tlrmkr
            _
            reqflush = do
                let buf = confWriteBuffer `plusPtr` off
                (mtrailers, flag) <- do
                    Trailers trailers <- tlrmkr Nothing
                    if null trailers
                        then return (Nothing, setEndStream defaultFlags)
                        else return (Just trailers, defaultFlags)
                -- Avoid sending an empty data frame before trailers at the end
                -- of a stream
                off' <-
                    if datPayloadLen /= 0 || isNothing mtrailers
                        then do
                            decreaseWindowSize ctx strm datPayloadLen
                            fillFrameHeader FrameData datPayloadLen streamNumber flag buf
                            return $ off + frameHeaderLength + datPayloadLen
                        else
                            return off
                off'' <- handleTrailers mtrailers off'
                halfClosedLocal ctx strm Finished
                if reqflush
                    then do
                        flushN off''
                        return (0, Nothing)
                    else return (off'', Nothing)
              where
                handleTrailers Nothing off0 = return off0
                handleTrailers (Just trailers) off0 = do
                    (ths, _) <- toTokenHeaderTable trailers
                    headerContinue streamNumber ths True {- endOfStream -} off0
        fillDataHeader
            _
            off
            0
            (Just next)
            tlrmkr
            out
            reqflush = do
                let out' = out{outputType = ONext next tlrmkr}
                if reqflush
                    then do
                        flushN off
                        return (0, Just out')
                    else return (off, Just out')
        fillDataHeader
            strm@Stream{streamNumber}
            off
            datPayloadLen
            (Just next)
            tlrmkr
            out
            reqflush = do
                let buf = confWriteBuffer `plusPtr` off
                    off' = off + frameHeaderLength + datPayloadLen
                    flag = defaultFlags
                fillFrameHeader FrameData datPayloadLen streamNumber flag buf
                decreaseWindowSize ctx strm datPayloadLen
                let out' = out{outputType = ONext next tlrmkr}
                if reqflush
                    then do
                        flushN off'
                        return (0, Just out')
                    else return (off', Just out')

        ----------------------------------------------------------------
        pushPromise :: StreamId -> StreamId -> TokenHeaderList -> Offset -> IO Int
        pushPromise pid sid ths off = do
            let offsid = off + frameHeaderLength -- checkme
                bufsid = confWriteBuffer `plusPtr` offsid
            poke32 (fromIntegral sid) bufsid 0
            let offkv = offsid + 4
                bufkv = confWriteBuffer `plusPtr` offkv
                limkv = confBufferSize - offkv
            (_, kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
            let flag = setEndHeader defaultFlags -- No EndStream flag
                buf = confWriteBuffer `plusPtr` off
                len = kvlen + 4
            fillFrameHeader FramePushPromise len pid flag buf
            return len

        ----------------------------------------------------------------
        {-# INLINE fillFrameHeader #-}
        fillFrameHeader :: FrameType -> Int -> StreamId -> FrameFlags -> Buffer -> IO ()
        fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
          where
            hinfo =
                FrameHeader
                    { payloadLength = len
                    , flags = flag
                    , streamId = sid
                    }

        setSenderDone = atomically $ writeTVar senderDone True

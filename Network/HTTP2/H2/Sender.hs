{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP2.H2.Sender (
    frameSender,
) where

import Control.Concurrent.MVar (putMVar)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Foreign.Ptr (minusPtr, plusPtr)
import Network.ByteOrder
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.IO
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports
import Network.HPACK (setLimitForEncoding, toTokenHeaderTable)
import Network.HTTP2.Frame
import Network.HTTP2.H2.Context
import Network.HTTP2.H2.EncodeFrame
import Network.HTTP2.H2.HPACK
import Network.HTTP2.H2.Manager hiding (start)
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Settings
import Network.HTTP2.H2.Stream
import Network.HTTP2.H2.StreamTable
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window

----------------------------------------------------------------

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    checkSTM (not isEmpty)

data Switch
    = C Control
    | O (Output Stream)
    | Flush

wrapException :: E.SomeException -> IO ()
wrapException se
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

frameSender :: Context -> Config -> Manager -> IO ()
frameSender
    ctx@Context{outputQ, controlQ, encodeDynamicTable, outputBufferLimit}
    Config{..}
    mgr = loop 0 `E.catch` wrapException
      where
        ----------------------------------------------------------------
        loop :: Offset -> IO ()
        loop off = do
            x <- atomically $ dequeue off
            case x of
                C ctl -> flushN off >> control ctl >> loop 0
                O out -> outputOrEnqueueAgain out off >>= flushIfNecessary >>= loop
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
                        then if off /= 0 then return Flush else retrySTM
                        else O <$> readTQueue outputQ
                else C <$> readTQueue controlQ

        ----------------------------------------------------------------
        copyAll [] buf = return buf
        copyAll (x : xs) buf = copy buf x >>= copyAll xs

        -- called with off == 0
        control :: Control -> IO ()
        control (CFinish e) = E.throwIO e
        control (CGoaway bs mvar) = do
            buf <- copyAll [bs] confWriteBuffer
            let off = buf `minusPtr` confWriteBuffer
            flushN off
            putMVar mvar ()
            E.throwIO GoAwayIsSent
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
        output :: Output Stream -> Offset -> WindowSize -> IO Offset
        output out@(Output strm OutObj{} (ONext curr tlrmkr) _ sentinel) off0 lim = do
            -- Data frame payload
            buflim <- readIORef outputBufferLimit
            let payloadOff = off0 + frameHeaderLength
                datBuf = confWriteBuffer `plusPtr` payloadOff
                datBufSiz = buflim - payloadOff
            Next datPayloadLen reqflush mnext <- curr datBuf (min datBufSiz lim)
            NextTrailersMaker tlrmkr' <- runTrailersMaker tlrmkr datBuf datPayloadLen
            fillDataHeaderEnqueueNext
                strm
                off0
                datPayloadLen
                mnext
                tlrmkr'
                sentinel
                out
                reqflush
        output (Output strm obj OObj mtbq sentinel) off0 _lim = do
            outputObj strm obj mtbq sentinel off0
        output out@(Output strm _ (OPush ths pid) _ _) off0 lim = do
            -- Creating a push promise header
            -- Frame id should be associated stream id from the client.
            let sid = streamNumber strm
            len <- pushPromise pid sid ths off0
            off <- flushIfNecessary $ off0 + frameHeaderLength + len
            output out{outputType = OObj} off lim
        output _ _ _ = undefined -- never reach

        ----------------------------------------------------------------
        outputObj
            :: Stream
            -> OutObj
            -> Maybe (TBQueue StreamingChunk)
            -> IO ()
            -> Offset
            -> IO Offset
        outputObj strm obj@(OutObj hdr body tlrmkr) mtbq sentinel off0 = do
            -- Header frame and Continuation frame
            let sid = streamNumber strm
                endOfStream = case body of
                    OutBodyNone -> True
                    _ -> False
            (ths, _) <- toTokenHeaderTable $ fixHeaders hdr
            off' <- headerContinue sid ths endOfStream off0
            -- halfClosedLocal calls closed which removes
            -- the stream from stream table.
            when endOfStream $ halfClosedLocal ctx strm Finished
            off <- flushIfNecessary off'
            let setOutputType otyp = Output strm obj otyp mtbq sentinel
            case body of
                OutBodyNone -> return off
                OutBodyFile (FileSpec path fileoff bytecount) -> do
                    (pread, sentinel') <- confPositionReadMaker path
                    refresh <- case sentinel' of
                        Closer closer -> timeoutClose mgr closer
                        Refresher refresher -> return refresher
                    let next = fillFileBodyGetNext pread fileoff bytecount refresh
                        out' = setOutputType $ ONext next tlrmkr
                    outputOrEnqueueAgain out' off
                OutBodyBuilder builder -> do
                    let next = fillBuilderBodyGetNext builder
                        out' = setOutputType $ ONext next tlrmkr
                    outputOrEnqueueAgain out' off
                OutBodyStreaming _ -> do
                    let out' = setOutputType $ nextForStreaming mtbq tlrmkr
                    outputOrEnqueueAgain out' off
                OutBodyStreamingUnmask _ -> do
                    let out' = setOutputType $ nextForStreaming mtbq tlrmkr
                    outputOrEnqueueAgain out' off

        ----------------------------------------------------------------
        nextForStreaming
            :: Maybe (TBQueue StreamingChunk)
            -> TrailersMaker
            -> OutputType
        nextForStreaming mtbq tlrmkr =
            let tbq = fromJust mtbq
                takeQ = atomically $ tryReadTBQueue tbq
                next = fillStreamBodyGetNext takeQ
             in ONext next tlrmkr

        ----------------------------------------------------------------
        outputOrEnqueueAgain :: Output Stream -> Offset -> IO Offset
        outputOrEnqueueAgain out@(Output strm obj otyp mtbq sentinel) off = E.handle resetStream $ do
            state <- readStreamState strm
            if isHalfClosedLocal state
                then return off
                else case otyp of
                    OWait wait -> do
                        -- Checking if all push are done.
                        forkAndEnqueueWhenReady wait outputQ out{outputType = OObj} mgr
                        return off
                    OObj ->
                        -- Send headers immediately, without waiting for data
                        -- No need to check the streaming window (applies to DATA frames only)
                        outputObj strm obj mtbq sentinel off
                    _ -> case mtbq of
                        Just tbq -> checkStreaming tbq
                        _ -> checkStreamWindowSize
          where
            checkStreaming tbq = do
                isEmpty <- atomically $ isEmptyTBQueue tbq
                if isEmpty
                    then do
                        forkAndEnqueueWhenReady (waitStreaming tbq) outputQ out mgr
                        return off
                    else checkStreamWindowSize
            -- FLOW CONTROL: WINDOW_UPDATE: send: respecting peer's limit
            checkStreamWindowSize = do
                sws <- getStreamWindowSize strm
                if sws == 0
                    then do
                        forkAndEnqueueWhenReady (waitStreamWindowSize strm) outputQ out mgr
                        return off
                    else do
                        cws <- getConnectionWindowSize ctx -- not 0
                        let lim = min cws sws
                        output out off lim
            resetStream e = do
                closed ctx strm (ResetByMe e)
                let rst = resetFrame InternalError $ streamNumber strm
                enqueueControl controlQ $ CFrames Nothing [rst]
                return off

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
        fillDataHeaderEnqueueNext
            :: Stream
            -> Offset
            -> Int
            -> Maybe DynaNext
            -> (Maybe ByteString -> IO NextTrailersMaker)
            -> IO ()
            -> Output Stream
            -> Bool
            -> IO Offset
        fillDataHeaderEnqueueNext
            strm@Stream{streamNumber}
            off
            datPayloadLen
            Nothing
            tlrmkr
            tell
            _
            reqflush = do
                let buf = confWriteBuffer `plusPtr` off
                    off' = off + frameHeaderLength + datPayloadLen
                (mtrailers, flag) <- do
                    Trailers trailers <- tlrmkr Nothing
                    if null trailers
                        then return (Nothing, setEndStream defaultFlags)
                        else return (Just trailers, defaultFlags)
                fillFrameHeader FrameData datPayloadLen streamNumber flag buf
                off'' <- handleTrailers mtrailers off'
                void tell
                halfClosedLocal ctx strm Finished
                decreaseWindowSize ctx strm datPayloadLen
                if reqflush
                    then do
                        flushN off''
                        return 0
                    else return off''
              where
                handleTrailers Nothing off0 = return off0
                handleTrailers (Just trailers) off0 = do
                    (ths, _) <- toTokenHeaderTable trailers
                    headerContinue streamNumber ths True {- endOfStream -} off0
        fillDataHeaderEnqueueNext
            _
            off
            0
            (Just next)
            tlrmkr
            _
            out
            reqflush = do
                let out' = out{outputType = ONext next tlrmkr}
                enqueueOutput outputQ out'
                if reqflush
                    then do
                        flushN off
                        return 0
                    else return off
        fillDataHeaderEnqueueNext
            strm@Stream{streamNumber}
            off
            datPayloadLen
            (Just next)
            tlrmkr
            _
            out
            reqflush = do
                let buf = confWriteBuffer `plusPtr` off
                    off' = off + frameHeaderLength + datPayloadLen
                    flag = defaultFlags
                fillFrameHeader FrameData datPayloadLen streamNumber flag buf
                decreaseWindowSize ctx strm datPayloadLen
                let out' = out{outputType = ONext next tlrmkr}
                enqueueOutput outputQ out'
                if reqflush
                    then do
                        flushN off'
                        return 0
                    else return off'

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

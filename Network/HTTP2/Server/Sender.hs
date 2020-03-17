{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Server.Sender (frameSender) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import Data.IORef
import Foreign.Ptr (plusPtr)
import Network.ByteOrder

import Imports
import Network.HPACK (setLimitForEncoding, toHeaderTable)
import Network.HTTP2
import Network.HTTP2.Priority (isEmptySTM, dequeueSTM, Precedence)
import Network.HTTP2.Server.EncodeFrame
import Network.HTTP2.Server.HPACK
import Network.HTTP2.Server.Manager hiding (start)
import Network.HTTP2.Server.Types
import Network.HTTP2.Server.Queue
import Network.HTTP2.Server.Context
import Network.HTTP2.Server.Stream
import Network.HTTP2.Types

----------------------------------------------------------------

data Leftover = LZero
              | LOne B.BufferWriter
              | LTwo ByteString B.BufferWriter

----------------------------------------------------------------

{-# INLINE getStreamWindowSize #-}
getStreamWindowSize :: Stream -> IO WindowSize
getStreamWindowSize Stream{streamWindow} = readTVarIO streamWindow

{-# INLINE waitStreamWindowSize #-}
waitStreamWindowSize :: Stream -> IO ()
waitStreamWindowSize Stream{streamWindow} = atomically $ do
    w <- readTVar streamWindow
    check (w > 0)

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)

data Switch = C Control
            | O (StreamId,Precedence,Output)
            | Flush

frameSender :: Context -> Config -> Manager -> IO ()
frameSender ctx@Context{outputQ,controlQ,connectionWindow,encodeDynamicTable}
            conf@Config{..}
            mgr = loop 0 `E.catch` ignore
  where
    dequeue off = do
        isEmpty <- isEmptyTQueue controlQ
        if isEmpty then do
            w <- readTVar connectionWindow
            check (w > 0)
            emp <- isEmptySTM outputQ
            if emp then
                if off /= 0 then return Flush else retry
              else
                O <$> dequeueSTM outputQ
          else
            C <$> readTQueue controlQ

    hardLimit = confBufferSize - 512

    loop off = do
        x <- atomically $ dequeue off
        case x of
            C ctl -> do
                when (off /= 0) $ flushN off
                off' <- control ctl off
                when (off' >= 0) $ loop off'
            O (_,pre,out) -> do
                let strm = outputStream out
                writeIORef (streamPrecedence strm) pre
                off' <- outputOrEnqueueAgain out off
                case off' of
                    0                    -> loop 0
                    _ | off' > hardLimit -> flushN off' >> loop 0
                      | otherwise        -> loop off'
            Flush -> flushN off >> loop 0

    control CFinish         _ = return (-1)
    control (CGoaway frame) _ = confSendAll frame >> return (-1)
    control (CFrame frame)  _ = confSendAll frame >> return 0
    control (CSettings frame alist) _ = do
        confSendAll frame
        setLimit alist
        return 0
    control (CSettings0 frame1 frame2 alist) off = do -- off == 0, just in case
        let !buf = confWriteBuffer `plusPtr` off
            !off' = off + BS.length frame1 + BS.length frame2
        buf' <- copy buf frame1
        void $ copy buf' frame2
        setLimit alist
        return off'

    {-# INLINE setLimit #-}
    setLimit alist = case lookup SettingsHeaderTableSize alist of
        Nothing  -> return ()
        Just siz -> setLimitForEncoding siz encodeDynamicTable

    output out@(Output strm (OutObj _ _ _) (ONext curr tlrmkr) _ sentinel) off0 lim = do
        -- Data frame payload
        let !buf = confWriteBuffer `plusPtr` off0
            !siz = confBufferSize - off0
            !payloadOff = off0 + frameHeaderLength
        Next datPayloadLen mnext <- curr buf siz lim
        NextTrailersMaker !tlrmkr' <- runTrailersMaker tlrmkr payloadOff datPayloadLen
        fillDataHeaderEnqueueNext strm off0 datPayloadLen mnext tlrmkr' sentinel out

    output out@(Output strm (OutObj hdr body tlrmkr) OObj mtbq sentinel) off0 lim = do
        -- Header frame and Continuation frame
        let !sid = streamNumber strm
            !endOfStream = case body of
                OutBodyNone -> True
                _           -> False
        (ths,_) <- toHeaderTable $ fixHeaders hdr
        kvlen <- headerContinue sid ths endOfStream off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen
        case body of
            OutBodyNone -> do
                halfClosedLocal ctx strm Finished
                return off
            OutBodyFile (FileSpec path fileoff bytecount) -> do
                -- Data frame payload
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillFileBodyGetNext conf payloadOff lim path fileoff bytecount mgr confPositionReadMaker
                NextTrailersMaker !tlrmkr' <- runTrailersMaker tlrmkr payloadOff datPayloadLen
                fillDataHeaderEnqueueNext strm off datPayloadLen mnext tlrmkr' sentinel out
            OutBodyBuilder builder -> do
                -- Data frame payload
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillBuilderBodyGetNext conf payloadOff lim builder
                NextTrailersMaker !tlrmkr' <- runTrailersMaker tlrmkr payloadOff datPayloadLen
                fillDataHeaderEnqueueNext strm off datPayloadLen mnext tlrmkr' sentinel out
            OutBodyStreaming _ -> do
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillStreamBodyGetNext conf payloadOff lim (fromJust mtbq) strm
                NextTrailersMaker !tlrmkr' <- runTrailersMaker tlrmkr payloadOff datPayloadLen
                fillDataHeaderEnqueueNext strm off datPayloadLen mnext tlrmkr' sentinel out

    output out@(Output strm _ (OPush ths pid) _ _) off0 lim = do
        -- Creating a push promise header
        -- Frame id should be associated stream id from the client.
        let !sid = streamNumber strm
        len <- pushPromise pid sid ths off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + len
        output out{outputType=OObj} off lim

    output _ _ _ = undefined -- never reach

    outputOrEnqueueAgain :: Output -> Int -> IO Int
    outputOrEnqueueAgain out@(Output strm _ otyp _ _) off = E.handle resetStream $ do
        state <- readStreamState strm
        if isHalfClosedLocal state then
            return off
          else case otyp of
                 OWait wait -> do
                     -- Checking if all push are done.
                     forkAndEnqueueWhenReady wait outputQ out{outputType=OObj} mgr
                     return off
                 _ -> case mtbq of
                        Just tbq -> checkStreaming tbq
                        _        -> checkStreamWindowSize
      where
        mtbq = outputStrmQ out
        checkStreaming tbq = do
            isEmpty <- atomically $ isEmptyTBQueue tbq
            if isEmpty then do
                forkAndEnqueueWhenReady (waitStreaming tbq) outputQ out mgr
                return off
              else
                checkStreamWindowSize
        checkStreamWindowSize = do
            sws <- getStreamWindowSize strm
            if sws == 0 then do
                forkAndEnqueueWhenReady (waitStreamWindowSize strm) outputQ out mgr
                return off
              else do
                cws <- readTVarIO connectionWindow -- not 0
                let !lim = min cws sws
                output out off lim
        resetStream e = do
            closed ctx strm (ResetByMe e)
            let !rst = resetFrame InternalError $ streamNumber strm
            enqueueControl controlQ $ CFrame rst
            return off

    {-# INLINE flushN #-}
    -- Flush the connection buffer to the socket, where the first 'n' bytes of
    -- the buffer are filled.
    flushN :: Int -> IO ()
    flushN n = bufferIO confWriteBuffer n confSendAll

    headerContinue sid ths endOfStream off = do
        let !offkv = off + frameHeaderLength
        let !bufkv = confWriteBuffer `plusPtr` offkv
            !limkv = confBufferSize - offkv
        (hs,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let flag0 = case hs of
                [] -> setEndHeader defaultFlags
                _  -> defaultFlags
            flag = if endOfStream then setEndStream flag0 else flag0
        let buf = confWriteBuffer `plusPtr` off
        fillFrameHeader FrameHeaders kvlen sid flag buf
        continue sid kvlen hs

    !bufHeaderPayload = confWriteBuffer `plusPtr` frameHeaderLength
    !headerPayloadLim = confBufferSize - frameHeaderLength

    continue _   kvlen [] = return kvlen
    continue sid kvlen ths = do
        flushN $ kvlen + frameHeaderLength
        -- Now off is 0
        (ths', kvlen') <- hpackEncodeHeaderLoop ctx bufHeaderPayload headerPayloadLim ths
        when (ths == ths') $ E.throwIO $ ConnectionError CompressionError "cannot compress the header"
        let flag = case ths' of
                [] -> setEndHeader defaultFlags
                _  -> defaultFlags
        fillFrameHeader FrameContinuation kvlen' sid flag confWriteBuffer
        continue sid kvlen' ths'

    {-# INLINE sendHeadersIfNecessary #-}
    -- Send headers if there is not room for a 1-byte data frame, and return
    -- the offset of the next frame's first header byte.
    sendHeadersIfNecessary off
      -- True if the connection buffer has room for a 1-byte data frame.
      | off + frameHeaderLength < confBufferSize = return off
      | otherwise = do
          flushN off
          return 0

    fillDataHeaderEnqueueNext strm@Stream{streamWindow,streamNumber}
                   off datPayloadLen Nothing tlrmkr tell _ = do
        let !buf  = confWriteBuffer `plusPtr` off
            !off' = off + frameHeaderLength + datPayloadLen
        (mtrailers, flag) <- do
              Trailers !trailers <- tlrmkr Nothing
              if null trailers then
                  return (Nothing, setEndStream defaultFlags)
                else
                  return (Just trailers, defaultFlags)
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        off'' <- handleTrailers mtrailers off'
        void tell
        halfClosedLocal ctx strm Finished
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' streamWindow (subtract datPayloadLen)
        return off''
      where
        handleTrailers Nothing off0 = return off0
        handleTrailers (Just trailers) off0 = do
            (ths,_) <- toHeaderTable trailers
            kvlen <- headerContinue streamNumber ths True off0
            sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen

    fillDataHeaderEnqueueNext Stream{streamWindow,streamNumber}
                   off datPayloadLen (Just next) tlrmkr _ out = do
        let !buf  = confWriteBuffer `plusPtr` off
            !off' = off + frameHeaderLength + datPayloadLen
            flag  = defaultFlags
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' streamWindow (subtract datPayloadLen)
        let !out' = out { outputType = ONext next tlrmkr }
        enqueueOutput outputQ out'
        return off'

    pushPromise pid sid ths off = do
        let !offsid = off + frameHeaderLength
            !bufsid = confWriteBuffer `plusPtr` offsid
        poke32 (fromIntegral sid) bufsid 0
        let !offkv  = offsid + 4
            !bufkv  = confWriteBuffer `plusPtr` offkv
            !limkv  = confBufferSize - offkv
        (_,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let !flag = setEndHeader defaultFlags -- No EndStream flag
            !buf = confWriteBuffer `plusPtr` off
            !len = kvlen + 4
        fillFrameHeader FramePushPromise len pid flag buf
        return len

    {-# INLINE fillFrameHeader #-}
    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader len flag sid

    runTrailersMaker tlrmkr off siz = do
        let datBuf = confWriteBuffer `plusPtr` off
        bufferIO datBuf siz $ \bs -> tlrmkr (Just bs)

    {-# INLINE ignore #-}
    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

----------------------------------------------------------------

fillBuilderBodyGetNext :: Config -> Int -> WindowSize -> Builder -> IO Next
fillBuilderBodyGetNext Config{confWriteBuffer,confBufferSize}
                       off lim bb = do
    let datBuf = confWriteBuffer `plusPtr` off
        room = min (confBufferSize - off) lim
    (len, signal) <- B.runBuilder bb datBuf room
    return $ nextForBuilder len signal

fillFileBodyGetNext :: Config -> Int -> WindowSize -> FilePath -> FileOffset -> ByteCount -> Manager -> PositionReadMaker -> IO Next
fillFileBodyGetNext Config{confWriteBuffer,confBufferSize}
                    off lim path start bytecount mgr prmaker = do
    let datBuf = confWriteBuffer `plusPtr` off
        room = min (confBufferSize - off) lim
    (pread, sentinel) <- prmaker path
    refresh <- case sentinel of
      Closer closer       -> timeoutClose mgr closer
      Refresher refresher -> return refresher
    len <- pread start (mini room bytecount) datBuf
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytecount - len) refresh

----------------------------------------------------------------

fillStreamBodyGetNext :: Config -> Int -> WindowSize -> TBQueue RspStreaming -> Stream -> IO Next
fillStreamBodyGetNext Config{confWriteBuffer,confBufferSize}
                      off lim sq strm = do
    let datBuf = confWriteBuffer `plusPtr` off
        room = min (confBufferSize - off) lim
    (leftover, cont, len) <- runStreamBuilder datBuf room sq
    return $ nextForStream sq strm leftover cont len

----------------------------------------------------------------

fillBufBuilder :: Leftover -> DynaNext
fillBufBuilder leftover buf0 siz0 lim = do
    let payloadBuf = buf0 `plusPtr` frameHeaderLength
        room = min (siz0 - frameHeaderLength) lim
    case leftover of
        LZero -> error "fillBufBuilder: LZero"
        LOne writer -> do
            (len, signal) <- writer payloadBuf room
            getNext len signal
        LTwo bs writer
          | BS.length bs <= room -> do
              buf1 <- copy payloadBuf bs
              let len1 = BS.length bs
              (len2, signal) <- writer buf1 (room - len1)
              getNext (len1 + len2) signal
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room bs
              void $ copy payloadBuf bs1
              getNext room (B.Chunk bs2 writer)
  where
    getNext l s = return $ nextForBuilder l s

nextForBuilder :: BytesFilled -> B.Next -> Next
nextForBuilder len B.Done
    = Next len Nothing
nextForBuilder len (B.More _ writer)
    = Next len $ Just (fillBufBuilder (LOne writer))
nextForBuilder len (B.Chunk bs writer)
    = Next len $ Just (fillBufBuilder (LTwo bs writer))

----------------------------------------------------------------

runStreamBuilder :: Buffer -> BufferSize -> TBQueue RspStreaming
                 -> IO (Leftover, Bool, BytesFilled)
runStreamBuilder buf0 room0 sq = loop buf0 room0 0
  where
    loop !buf !room !total = do
        mbuilder <- atomically $ tryReadTBQueue sq
        case mbuilder of
            Nothing      -> return (LZero, True, total)
            Just (RSBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let !total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More  _ writer  -> return (LOne writer, True, total')
                    B.Chunk bs writer -> return (LTwo bs writer, True, total')
            Just RSFlush  -> return (LZero, True, total)
            Just RSFinish -> return (LZero, False, total)

fillBufStream :: Leftover -> TBQueue RspStreaming -> Stream -> DynaNext
fillBufStream leftover0 sq strm buf0 siz0 lim0 = do
    let payloadBuf = buf0 `plusPtr` frameHeaderLength
        room0 = min (siz0 - frameHeaderLength) lim0
    case leftover0 of
        LZero -> do
            (leftover, cont, len) <- runStreamBuilder payloadBuf room0 sq
            getNext leftover cont len
        LOne writer -> write writer payloadBuf room0 0
        LTwo bs writer
          | BS.length bs <= room0 -> do
              buf1 <- copy payloadBuf bs
              let len = BS.length bs
              write writer buf1 (room0 - len) len
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room0 bs
              void $ copy payloadBuf bs1
              getNext (LTwo bs2 writer) True room0
  where
    getNext l b r = return $ nextForStream sq strm l b r
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (leftover, cont, extra) <- runStreamBuilder (buf `plusPtr` len) (room - len) sq
                let !total = sofar + len + extra
                getNext leftover cont total
            B.More  _ writer -> do
                let !total = sofar + len
                getNext (LOne writer) True total
            B.Chunk bs writer -> do
                let !total = sofar + len
                getNext (LTwo bs writer) True total

nextForStream :: TBQueue RspStreaming -> Stream
              -> Leftover -> Bool -> BytesFilled
              -> Next
nextForStream _ _ _ False len = Next len Nothing
nextForStream sq strm leftOrZero True len =
    Next len $ Just (fillBufStream leftOrZero sq strm)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillBufFile pread start bytes refresh buf siz lim = do
    let payloadBuf = buf `plusPtr` frameHeaderLength
        room = min (siz - frameHeaderLength) lim
    len <- pread start (mini room bytes) payloadBuf
    refresh
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytes - len) refresh

nextForFile :: BytesFilled -> PositionRead -> FileOffset -> ByteCount -> IO () -> Next
nextForFile 0   _  _     _     _       = Next 0   Nothing
nextForFile len _  _     0     _       = Next len Nothing
nextForFile len pread start bytes refresh =
    Next len $ Just (fillBufFile pread start bytes refresh)

{-# INLINE mini #-}
mini :: Int -> Int64 -> Int64
mini i n
  | fromIntegral i < n = fromIntegral i
  | otherwise          = n

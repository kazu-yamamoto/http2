{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Sender (
    frameSender
  , fillBuilderBodyGetNext
  , fillFileBodyGetNext
  , fillStreamBodyGetNext
  , runTrailersMaker
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import Foreign.Ptr (plusPtr)
import Network.ByteOrder
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports
import Network.HPACK (setLimitForEncoding, toHeaderTable)
import Network.HTTP2.Arch.Config
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.File
import Network.HTTP2.Arch.HPACK
import Network.HTTP2.Arch.Manager hiding (start)
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame

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
    checkSTM (w > 0)

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    checkSTM (not isEmpty)

data Switch = C Control
            | O (Output Stream)
            | Flush

frameSender :: Context -> Config -> Manager -> IO ()
frameSender ctx@Context{outputQ,controlQ,connectionWindow,encodeDynamicTable}
            Config{..}
            mgr = loop 0
  where
    dequeue off = do
        isEmpty <- isEmptyTQueue controlQ
        if isEmpty then do
            w <- readTVar connectionWindow
            checkSTM (w > 0)
            emp <- isEmptyTQueue outputQ
            if emp then
                if off /= 0 then return Flush else retrySTM
              else
                O <$> readTQueue outputQ
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
            O out -> do
                off' <- outputOrEnqueueAgain out off
                case off' of
                    0                    -> loop 0
                    _ | off' > hardLimit -> flushN off' >> loop 0
                      | otherwise        -> loop off'
            Flush -> flushN off >> loop 0

    control (CFinish     e) _ = E.throwIO e
    control (CGoaway frame) _ = confSendAll frame >> return 0
    control (CFrame  frame) _ = confSendAll frame >> return 0
    control (CSettings frame alist) _ = do
        confSendAll frame
        setLimit alist
        return 0
    control (CSettings0 frame1 frame2 alist) off = do -- off == 0, just in case
        let buf = confWriteBuffer `plusPtr` off
            off' = off + BS.length frame1 + BS.length frame2
        buf' <- copy buf frame1
        void $ copy buf' frame2
        setLimit alist
        return off'

    {-# INLINE setLimit #-}
    setLimit alist = case lookup SettingsHeaderTableSize alist of
        Nothing  -> return ()
        Just siz -> setLimitForEncoding siz encodeDynamicTable

    output out@(Output strm OutObj{} (ONext curr tlrmkr) _ sentinel) off0 lim = do
        -- Data frame payload
        let payloadOff = off0 + frameHeaderLength
            datBuf     = confWriteBuffer `plusPtr` payloadOff
            datBufSiz  = confBufferSize - payloadOff
        Next datPayloadLen mnext <- curr datBuf datBufSiz lim -- checkme
        NextTrailersMaker tlrmkr' <- runTrailersMaker tlrmkr datBuf datPayloadLen
        fillDataHeaderEnqueueNext strm off0 datPayloadLen mnext tlrmkr' sentinel out

    output out@(Output strm (OutObj hdr body tlrmkr) OObj mtbq _) off0 lim = do
        -- Header frame and Continuation frame
        let sid = streamNumber strm
            endOfStream = case body of
                OutBodyNone -> True
                _           -> False
        (ths,_) <- toHeaderTable $ fixHeaders hdr
        kvlen <- headerContinue sid ths endOfStream off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen
        case body of
            OutBodyNone -> do
                -- halfClosedLocal calls closed which removes
                -- the stream from stream table.
                when (isServer ctx) $ halfClosedLocal ctx strm Finished
                return off
            OutBodyFile (FileSpec path fileoff bytecount) -> do
                (pread, sentinel') <- confPositionReadMaker path
                refresh <- case sentinel' of
                             Closer closer       -> timeoutClose mgr closer
                             Refresher refresher -> return refresher
                let next = fillFileBodyGetNext pread fileoff bytecount refresh
                    out' = out { outputType = ONext next tlrmkr }
                output out' off lim
            OutBodyBuilder builder -> do
                let next = fillBuilderBodyGetNext builder
                    out' = out { outputType = ONext next tlrmkr }
                output out' off lim
            OutBodyStreaming _ -> do
                let tbq = fromJust mtbq
                    takeQ = atomically $ tryReadTBQueue tbq
                    next = fillStreamBodyGetNext takeQ
                    out' = out { outputType = ONext next tlrmkr }
                output out' off lim

    output out@(Output strm _ (OPush ths pid) _ _) off0 lim = do
        -- Creating a push promise header
        -- Frame id should be associated stream id from the client.
        let sid = streamNumber strm
        len <- pushPromise pid sid ths off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + len
        output out{outputType=OObj} off lim

    output _ _ _ = undefined -- never reach

    outputOrEnqueueAgain :: Output Stream -> Int -> IO Int
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
                let lim = min cws sws
                output out off lim
        resetStream e = do
            closed ctx strm (ResetByMe e)
            let rst = resetFrame InternalError $ streamNumber strm
            enqueueControl controlQ $ CFrame rst
            return off

    {-# INLINE flushN #-}
    -- Flush the connection buffer to the socket, where the first 'n' bytes of
    -- the buffer are filled.
    flushN :: Int -> IO ()
    flushN n = bufferIO confWriteBuffer n confSendAll

    headerContinue sid ths endOfStream off = do
        let offkv = off + frameHeaderLength
        let bufkv = confWriteBuffer `plusPtr` offkv
            limkv = confBufferSize - offkv
        (hs,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let flag0 = case hs of
                [] -> setEndHeader defaultFlags
                _  -> defaultFlags
            flag = if endOfStream then setEndStream flag0 else flag0
        let buf = confWriteBuffer `plusPtr` off
        fillFrameHeader FrameHeaders kvlen sid flag buf
        continue sid kvlen hs

    bufHeaderPayload = confWriteBuffer `plusPtr` frameHeaderLength
    headerPayloadLim = confBufferSize - frameHeaderLength

    continue _   kvlen [] = return kvlen
    continue sid kvlen ths = do
        flushN $ kvlen + frameHeaderLength
        -- Now off is 0
        (ths', kvlen') <- hpackEncodeHeaderLoop ctx bufHeaderPayload headerPayloadLim ths
        when (ths == ths') $ E.throwIO $ ConnectionErrorIsSent CompressionError sid "cannot compress the header"
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
        let buf  = confWriteBuffer `plusPtr` off
            off' = off + frameHeaderLength + datPayloadLen
        (mtrailers, flag) <- do
              Trailers trailers <- tlrmkr Nothing
              if null trailers then
                  return (Nothing, setEndStream defaultFlags)
                else
                  return (Just trailers, defaultFlags)
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        off'' <- handleTrailers mtrailers off'
        void tell
        when (isServer ctx) $ halfClosedLocal ctx strm Finished
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' streamWindow (subtract datPayloadLen)
        return off''
      where
        handleTrailers Nothing off0 = return off0
        handleTrailers (Just trailers) off0 = do
            (ths,_) <- toHeaderTable trailers
            kvlen <- headerContinue streamNumber ths True off0
            sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen

    fillDataHeaderEnqueueNext _
                   off 0 (Just next) tlrmkr _ out = do
        let out' = out { outputType = ONext next tlrmkr }
        enqueueOutput outputQ out'
        return off

    fillDataHeaderEnqueueNext Stream{streamWindow,streamNumber}
                   off datPayloadLen (Just next) tlrmkr _ out = do
        let buf  = confWriteBuffer `plusPtr` off
            off' = off + frameHeaderLength + datPayloadLen
            flag  = defaultFlags
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' streamWindow (subtract datPayloadLen)
        let out' = out { outputType = ONext next tlrmkr }
        enqueueOutput outputQ out'
        return off'

    pushPromise pid sid ths off = do
        let offsid = off + frameHeaderLength -- checkme
            bufsid = confWriteBuffer `plusPtr` offsid
        poke32 (fromIntegral sid) bufsid 0
        let offkv  = offsid + 4
            bufkv  = confWriteBuffer `plusPtr` offkv
            limkv  = confBufferSize - offkv
        (_,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let flag = setEndHeader defaultFlags -- No EndStream flag
            buf = confWriteBuffer `plusPtr` off
            len = kvlen + 4
        fillFrameHeader FramePushPromise len pid flag buf
        return len

    {-# INLINE fillFrameHeader #-}
    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader len flag sid

-- | Running trailers-maker.
--
-- > bufferIO buf siz $ \bs -> tlrmkr (Just bs)
runTrailersMaker :: TrailersMaker -> Buffer -> Int -> IO NextTrailersMaker
runTrailersMaker tlrmkr buf siz = bufferIO buf siz $ \bs -> tlrmkr (Just bs)

----------------------------------------------------------------

fillBuilderBodyGetNext :: Builder -> DynaNext
fillBuilderBodyGetNext bb buf siz lim = do
    let room = min siz lim
    (len, signal) <- B.runBuilder bb buf room
    return $ nextForBuilder len signal

fillFileBodyGetNext :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillFileBodyGetNext pread start bytecount refresh buf siz lim = do
    let room = min siz lim
    len <- pread start (mini room bytecount) buf
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytecount - len) refresh

fillStreamBodyGetNext :: IO (Maybe StreamingChunk) -> DynaNext
fillStreamBodyGetNext takeQ buf siz lim = do
    let room = min siz lim
    (leftover, cont, len) <- runStreamBuilder buf room takeQ
    return $ nextForStream takeQ leftover cont len

----------------------------------------------------------------

fillBufBuilder :: Leftover -> DynaNext
fillBufBuilder leftover buf0 siz0 lim = do
    let room = min siz0 lim
    case leftover of
        LZero -> error "fillBufBuilder: LZero"
        LOne writer -> do
            (len, signal) <- writer buf0 room
            getNext len signal
        LTwo bs writer
          | BS.length bs <= room -> do
              buf1 <- copy buf0 bs
              let len1 = BS.length bs
              (len2, signal) <- writer buf1 (room - len1)
              getNext (len1 + len2) signal
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room bs
              void $ copy buf0 bs1
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

runStreamBuilder :: Buffer -> BufferSize -> IO (Maybe StreamingChunk)
                 -> IO (Leftover, Bool, BytesFilled)
runStreamBuilder buf0 room0 takeQ = loop buf0 room0 0
  where
    loop buf room total = do
        mbuilder <- takeQ
        case mbuilder of
            Nothing      -> return (LZero, True, total)
            Just (StreamingBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More  _ writer  -> return (LOne writer, True, total')
                    B.Chunk bs writer -> return (LTwo bs writer, True, total')
            Just StreamingFlush       -> return (LZero, True, total)
            Just StreamingFinished    -> return (LZero, False, total)

fillBufStream :: Leftover -> IO (Maybe StreamingChunk) -> DynaNext
fillBufStream leftover0 takeQ buf0 siz0 lim0 = do
    let room0 = min siz0 lim0
    case leftover0 of
        LZero -> do
            (leftover, cont, len) <- runStreamBuilder buf0 room0 takeQ
            getNext leftover cont len
        LOne writer -> write writer buf0 room0 0
        LTwo bs writer
          | BS.length bs <= room0 -> do
              buf1 <- copy buf0 bs
              let len = BS.length bs
              write writer buf1 (room0 - len) len
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room0 bs
              void $ copy buf0 bs1
              getNext (LTwo bs2 writer) True room0
  where
    getNext l b r = return $ nextForStream takeQ l b r
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (leftover, cont, extra) <- runStreamBuilder (buf `plusPtr` len) (room - len) takeQ
                let total = sofar + len + extra
                getNext leftover cont total
            B.More  _ writer -> do
                let total = sofar + len
                getNext (LOne writer) True total
            B.Chunk bs writer -> do
                let total = sofar + len
                getNext (LTwo bs writer) True total

nextForStream :: IO (Maybe StreamingChunk)
              -> Leftover -> Bool -> BytesFilled
              -> Next
nextForStream _ _ False len = Next len Nothing
nextForStream takeQ leftOrZero True len =
    Next len $ Just (fillBufStream leftOrZero takeQ)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillBufFile pread start bytes refresh buf siz lim = do
    let room = min siz lim
    len <- pread start (mini room bytes) buf
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

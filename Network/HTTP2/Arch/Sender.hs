{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.IORef (readIORef, writeIORef)
import Foreign.Ptr (plusPtr, minusPtr)
import Network.ByteOrder
import qualified UnliftIO.Exception as E
import UnliftIO.STM

import Imports
import Network.HPACK (setLimitForEncoding, toHeaderTable, TokenHeaderList)
import Network.HTTP2.Arch.Config
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.File
import Network.HTTP2.Arch.HPACK
import Network.HTTP2.Arch.Manager hiding (start)
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Types
import Network.HTTP2.Arch.Window
import Network.HTTP2.Frame

----------------------------------------------------------------

data Leftover = LZero
              | LOne B.BufferWriter
              | LTwo ByteString B.BufferWriter

----------------------------------------------------------------

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    checkSTM (not isEmpty)

data Switch = C Control
            | O (Output Stream)
            | Flush

wrapException :: E.SomeException -> IO ()
wrapException se
  | Just (e :: HTTP2Error) <- E.fromException se = E.throwIO e
  | otherwise = E.throwIO $ BadThingHappen se

frameSender :: Context -> Config -> Manager -> IO ()
frameSender ctx@Context{outputQ,controlQ,encodeDynamicTable,outputBufferLimit}
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
        if off <= buflim - 512 then
            return off
          else do
            flushN off
            return 0

    dequeue :: Offset -> STM Switch
    dequeue off = do
        isEmpty <- isEmptyTQueue controlQ
        if isEmpty then do
            waitConnectionWindowSize ctx
            emp <- isEmptyTQueue outputQ
            if emp then
                if off /= 0 then return Flush else retrySTM
              else
                O <$> readTQueue outputQ
          else
            C <$> readTQueue controlQ

    ----------------------------------------------------------------
    copyAll []     buf = return buf
    copyAll (x:xs) buf = copy buf x >>= copyAll xs

    -- called with off == 0
    control :: Control -> IO ()
    control (CFinish     e) = E.throwIO e
    control (CFrames ms xs) = do
        buf <- copyAll xs confWriteBuffer
        let off = buf `minusPtr` confWriteBuffer
        flushN off
        case ms of
          Nothing    -> return ()
          Just peerAlist -> do
              -- Peer SETTINGS_INITIAL_WINDOW_SIZE
              updatePeerSettings ctx peerAlist
              -- Peer SETTINGS_MAX_FRAME_SIZE
              case lookup SettingsMaxFrameSize peerAlist of
                Nothing -> return ()
                Just payloadLen -> do
                    let dlim = payloadLen + frameHeaderLength
                        buflim | confBufferSize >= dlim = dlim
                               | otherwise              = confBufferSize
                    writeIORef outputBufferLimit buflim
              -- Peer SETTINGS_HEADER_TABLE_SIZE
              case lookup SettingsHeaderTableSize peerAlist of
                Nothing  -> return ()
                Just siz -> setLimitForEncoding siz encodeDynamicTable

    ----------------------------------------------------------------
    output :: Output Stream -> Offset -> WindowSize -> IO Offset
    output out@(Output strm OutObj{} (ONext curr tlrmkr) _ sentinel) off0 lim = do
        -- Data frame payload
        buflim <- readIORef outputBufferLimit
        let payloadOff = off0 + frameHeaderLength
            datBuf     = confWriteBuffer `plusPtr` payloadOff
            datBufSiz  = buflim - payloadOff
        Next datPayloadLen reqflush mnext <- curr datBuf datBufSiz lim -- checkme
        NextTrailersMaker tlrmkr' <- runTrailersMaker tlrmkr datBuf datPayloadLen
        fillDataHeaderEnqueueNext strm off0 datPayloadLen mnext tlrmkr' sentinel out reqflush

    output out@(Output strm (OutObj hdr body tlrmkr) OObj mtbq _) off0 lim = do
        -- Header frame and Continuation frame
        let sid = streamNumber strm
            endOfStream = case body of
                OutBodyNone -> True
                _           -> False
        (ths,_) <- toHeaderTable $ fixHeaders hdr
        off' <- headerContinue sid ths endOfStream off0
        off <- flushIfNecessary off'
        case body of
            OutBodyNone -> do
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
            OutBodyStreaming _ ->
                output (setNextForStreaming mtbq tlrmkr out) off lim
            OutBodyStreamingUnmask _ ->
                output (setNextForStreaming mtbq tlrmkr out) off lim

    output out@(Output strm _ (OPush ths pid) _ _) off0 lim = do
        -- Creating a push promise header
        -- Frame id should be associated stream id from the client.
        let sid = streamNumber strm
        len <- pushPromise pid sid ths off0
        off <- flushIfNecessary $ off0 + frameHeaderLength + len
        output out{outputType=OObj} off lim

    output _ _ _ = undefined -- never reach

    ----------------------------------------------------------------
    setNextForStreaming :: Maybe (TBQueue StreamingChunk) -> TrailersMaker -> Output Stream -> Output Stream
    setNextForStreaming mtbq tlrmkr out =
        let tbq = fromJust mtbq
            takeQ = atomically $ tryReadTBQueue tbq
            next = fillStreamBodyGetNext takeQ
        in out { outputType = ONext next tlrmkr }

    ----------------------------------------------------------------
    outputOrEnqueueAgain :: Output Stream -> Offset -> IO Offset
    outputOrEnqueueAgain out@(Output strm _ otyp _ _) off = E.handle resetStream $ do
        case otyp of
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
        (ths,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths0
        if kvlen == 0 then
            continue off0 ths FrameHeaders
          else do
            let flag = getFlag ths
                buf = confWriteBuffer `plusPtr` off0
                off = offkv + kvlen
            fillFrameHeader FrameHeaders kvlen sid flag buf
            continue off ths FrameContinuation
      where
        eos = if endOfStream then setEndStream else id
        getFlag [] = eos $ setEndHeader defaultFlags
        getFlag _  = eos $ defaultFlags

        continue :: Offset -> TokenHeaderList -> FrameType -> IO Offset
        continue off [] _ = return off
        continue off ths ft = do
            flushN off
            -- Now off is 0
            buflim <- readIORef outputBufferLimit
            let bufHeaderPayload = confWriteBuffer `plusPtr` frameHeaderLength

                headerPayloadLim = buflim - frameHeaderLength
            (ths', kvlen') <- hpackEncodeHeaderLoop ctx bufHeaderPayload headerPayloadLim ths
            when (ths == ths') $ E.throwIO $ ConnectionErrorIsSent CompressionError sid "cannot compress the header"
            let flag = getFlag ths'
                off' = frameHeaderLength + kvlen'
            fillFrameHeader ft kvlen' sid flag confWriteBuffer
            continue off' ths' FrameContinuation

    ----------------------------------------------------------------
    fillDataHeaderEnqueueNext :: Stream
                              -> Offset
                              -> Int
                              -> Maybe DynaNext
                              -> (Maybe ByteString -> IO NextTrailersMaker)
                              -> IO ()
                              -> Output Stream
                              -> Bool
                              -> IO Offset
    fillDataHeaderEnqueueNext strm@Stream{streamNumber}
                   off datPayloadLen Nothing tlrmkr tell _ reqflush = do
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
        decreaseWindowSize ctx strm datPayloadLen
        if reqflush then do
            flushN off''
            return 0
          else
            return off''
      where
        handleTrailers Nothing off0 = return off0
        handleTrailers (Just trailers) off0 = do
            (ths,_) <- toHeaderTable trailers
            headerContinue streamNumber ths True off0

    fillDataHeaderEnqueueNext _
                   off 0 (Just next) tlrmkr _ out reqflush = do
        let out' = out { outputType = ONext next tlrmkr }
        enqueueOutput outputQ out'
        if reqflush then do
            flushN off
            return 0
          else
            return off

    fillDataHeaderEnqueueNext strm@Stream{streamNumber}
                   off datPayloadLen (Just next) tlrmkr _ out reqflush = do
        let buf  = confWriteBuffer `plusPtr` off
            off' = off + frameHeaderLength + datPayloadLen
            flag  = defaultFlags
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        decreaseWindowSize ctx strm datPayloadLen
        let out' = out { outputType = ONext next tlrmkr }
        enqueueOutput outputQ out'
        if reqflush then do
            flushN off'
            return 0
          else
            return off'

    ----------------------------------------------------------------
    pushPromise :: StreamId -> StreamId -> TokenHeaderList -> Offset -> IO Int
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

    ----------------------------------------------------------------
    {-# INLINE fillFrameHeader #-}
    fillFrameHeader :: FrameType -> Int -> StreamId -> FrameFlags -> Buffer -> IO ()
    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader {
            payloadLength = len
          , flags         = flag
          , streamId      = sid
          }

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
    (cont, len, reqflush, leftover) <- runStreamBuilder buf room takeQ
    return $ nextForStream cont len reqflush leftover takeQ

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
    = Next len True Nothing -- let's flush
nextForBuilder len (B.More _ writer)
    = Next len False $ Just (fillBufBuilder (LOne writer))
nextForBuilder len (B.Chunk bs writer)
    = Next len False $ Just (fillBufBuilder (LTwo bs writer))

----------------------------------------------------------------

runStreamBuilder :: Buffer -> BufferSize -> IO (Maybe StreamingChunk)
                 -> IO (Bool  -- continue
                       ,BytesFilled
                       ,Bool  -- require flusing
                       ,Leftover)
runStreamBuilder buf0 room0 takeQ = loop buf0 room0 0
  where
    loop buf room total = do
        mbuilder <- takeQ
        case mbuilder of
            Nothing      -> return (True, total, False, LZero)
            Just (StreamingBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More  _ writer  -> return (True,  total', False, LOne writer)
                    B.Chunk bs writer -> return (True,  total', False, LTwo bs writer)
            Just StreamingFlush       -> return (True,  total,  True,  LZero)
            Just StreamingFinished    -> return (False, total,  True,  LZero)

fillBufStream :: Leftover -> IO (Maybe StreamingChunk) -> DynaNext
fillBufStream leftover0 takeQ buf0 siz0 lim0 = do
    let room0 = min siz0 lim0
    case leftover0 of
        LZero -> do
            (cont, len, reqflush, leftover) <- runStreamBuilder buf0 room0 takeQ
            getNext cont len reqflush leftover
        LOne writer -> write writer buf0 room0 0
        LTwo bs writer
          | BS.length bs <= room0 -> do
              buf1 <- copy buf0 bs
              let len = BS.length bs
              write writer buf1 (room0 - len) len
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room0 bs
              void $ copy buf0 bs1
              getNext True room0 False $ LTwo bs2 writer
  where
    getNext :: Bool -> BytesFilled -> Bool -> Leftover -> IO Next
    getNext cont len reqflush l = return $ nextForStream cont len reqflush l takeQ

    write :: (Buffer -> BufferSize -> IO (Int, B.Next))
          -> Buffer -> BufferSize -> Int
          -> IO Next
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (cont, extra, reqflush, leftover) <- runStreamBuilder (buf `plusPtr` len) (room - len) takeQ
                let total = sofar + len + extra
                getNext cont total reqflush leftover
            B.More  _ writer -> do
                let total = sofar + len
                getNext True total False $ LOne writer
            B.Chunk bs writer -> do
                let total = sofar + len
                getNext True total False $ LTwo bs writer

nextForStream :: Bool -> BytesFilled -> Bool
              -> Leftover -> IO (Maybe StreamingChunk)
              -> Next
nextForStream False len reqflush _          _     = Next len reqflush Nothing
nextForStream True  len reqflush leftOrZero takeQ =
    Next len reqflush $ Just (fillBufStream leftOrZero takeQ)

----------------------------------------------------------------

fillBufFile :: PositionRead -> FileOffset -> ByteCount -> IO () -> DynaNext
fillBufFile pread start bytes refresh buf siz lim = do
    let room = min siz lim
    len <- pread start (mini room bytes) buf
    refresh
    let len' = fromIntegral len
    return $ nextForFile len' pread (start + len) (bytes - len) refresh

nextForFile :: BytesFilled -> PositionRead -> FileOffset -> ByteCount -> IO () -> Next
nextForFile 0   _  _     _     _       = Next 0   True  Nothing -- let's flush
nextForFile len _  _     0     _       = Next len False Nothing
nextForFile len pread start bytes refresh =
    Next len False $ Just $ fillBufFile pread start bytes refresh

{-# INLINE mini #-}
mini :: Int -> Int64 -> Int64
mini i n
  | fromIntegral i < n = fromIntegral i
  | otherwise          = n

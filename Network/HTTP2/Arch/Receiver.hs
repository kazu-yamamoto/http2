{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Receiver (
    frameReceiver
  , maxConcurrency
  , initialFrame
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef

import Imports hiding (delete, insert)
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2.Arch.Context
import Network.HTTP2.Arch.EncodeFrame
import Network.HTTP2.Arch.HPACK
import Network.HTTP2.Arch.Queue
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame
import Network.HTTP2.Priority (toPrecedence, delete, prepare)

----------------------------------------------------------------

maxConcurrency :: Int
maxConcurrency = recommendedConcurrency

initialFrame :: ByteString
initialFrame = settingsFrame id [(SettingsMaxConcurrentStreams,maxConcurrency)]

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(IO ByteString)

mkSource :: IO ByteString -> IO Source
mkSource func = do
    ref <- newIORef BS.empty
    return $! Source ref func

readSource :: Source -> IO ByteString
readSource (Source ref func) = do
    bs <- readIORef ref
    if BS.null bs
        then func
        else do
            writeIORef ref BS.empty
            return bs

----------------------------------------------------------------

type RecvN = Int -> IO ByteString

frameReceiver :: Context -> RecvN -> IO ()
frameReceiver ctx@Context{..} recvN = loop 0 `E.catch` sendGoaway
  where
    loop :: Int -> IO ()
    loop n
      | n == 6 = do
          yield
          loop 0
      | otherwise = do
        hd <- recvN frameHeaderLength
        if BS.null hd then
            enqueueControl controlQ CFinish
          else do
            cont <- processFrame ctx recvN $ decodeFrameHeader hd
            when cont $ loop (n + 1)

    sendGoaway e
      | Just (ConnectionError err msg) <- E.fromException e = do
          psid <- getPeerStreamID ctx
          let frame = goawayFrame psid err msg
          enqueueControl controlQ $ CGoaway frame
      | otherwise = return ()

----------------------------------------------------------------

processFrame :: Context -> RecvN -> (FrameTypeId, FrameHeader) -> IO Bool
processFrame _ctx _recvN (fid, FrameHeader{streamId})
  | isServerInitiated streamId &&
    (fid `notElem` [FramePriority,FrameRSTStream,FrameWindowUpdate]) =
    E.throwIO $ ConnectionError ProtocolError "stream id should be odd"
processFrame Context{..} recvN (FrameUnknown _, FrameHeader{payloadLength}) = do
    mx <- readIORef continued
    case mx of
        Nothing -> do
            -- ignoring unknown frame
            void $ recvN payloadLength
            return True
        Just _  -> E.throwIO $ ConnectionError ProtocolError "unknown frame"
processFrame ctx _recvN (FramePushPromise, _)
  | isServer ctx = E.throwIO $ ConnectionError ProtocolError "push promise is not allowed"
processFrame ctx@Context{..} recvN typhdr@(ftyp, header@FrameHeader{payloadLength}) = do
    settings <- readIORef http2settings
    case checkFrameHeader settings typhdr of
      Left h2err -> case h2err of
          StreamError err sid -> do
              sendReset err sid
              void $ recvN payloadLength
              return True
          connErr -> E.throwIO connErr
      Right _ -> do
          ex <- E.try $ controlOrStream ctx recvN ftyp header
          case ex of
              Left (StreamError err sid) -> do
                  sendReset err sid
                  return True
              Left connErr -> E.throw connErr
              Right cont -> return cont
  where
    sendReset err sid = do
        let frame = resetFrame err sid
        enqueueControl controlQ $ CFrame frame

----------------------------------------------------------------

controlOrStream :: Context -> RecvN -> FrameTypeId -> FrameHeader -> IO Bool
controlOrStream ctx@Context{..} recvN ftyp header@FrameHeader{streamId, payloadLength}
  | isControl streamId = do
      pl <- recvN payloadLength
      control ftyp header pl ctx
  | otherwise = do
      checkContinued
      mstrm <- getStream ctx ftyp streamId
      pl <- recvN payloadLength
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
      return True
  where
    setContinued   = writeIORef continued $ Just streamId
    resetContinued = writeIORef continued Nothing
    checkContinued = do
        mx <- readIORef continued
        case mx of
            Nothing  -> return ()
            Just sid
              | sid == streamId && ftyp == FrameContinuation -> return ()
              | otherwise -> E.throwIO $ ConnectionError ProtocolError "continuation frame must follow"

----------------------------------------------------------------

processState :: StreamState -> Context -> Stream -> StreamId -> IO Bool
processState (Open (NoBody tbl@(_,reqvt) pri)) ctx@Context{..} strm@Stream{streamPrecedence,streamInput} streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    when (just mcl (/= (0 :: Int))) $ E.throwIO $ StreamError ProtocolError streamId
    writeIORef streamPrecedence $ toPrecedence pri
    halfClosedRemote ctx strm
    tlr <- newIORef Nothing
    let inpObj = InpObj tbl (Just 0) (return "") tlr
    if isServer ctx then do
        atomically $ writeTQueue inputQ $ Input strm inpObj
      else
        putMVar streamInput inpObj
    return False
processState (Open (HasBody tbl@(_,reqvt) pri)) ctx@Context{..} strm@Stream{streamPrecedence,streamInput} _streamId = do
    q <- newTQueueIO
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    writeIORef streamPrecedence $ toPrecedence pri
    bodyLength <- newIORef 0
    tlr <- newIORef Nothing
    setStreamState ctx strm $ Open (Body q mcl bodyLength tlr)
    readQ <- newReadBody q
    bodySource <- mkSource readQ
    let inpObj = InpObj tbl mcl (readSource bodySource) tlr
    if isServer ctx then
        atomically $ writeTQueue inputQ $ Input strm inpObj
      else
        putMVar streamInput inpObj
    return False
processState s@(Open Continued{}) ctx@Context{..} strm _streamId = do
    setStreamState ctx strm s
    return True
processState HalfClosedRemote ctx@Context{..} strm _streamId = do
    halfClosedRemote ctx strm
    return False
processState s ctx@Context{..} strm _streamId = do
    -- Idle, Open Body, Closed
    setStreamState ctx strm s
    return False

----------------------------------------------------------------

getStream :: Context -> FrameTypeId -> StreamId -> IO (Maybe Stream)
getStream ctx@Context{..} ftyp streamId =
    search streamTable streamId >>= getStream' ctx ftyp streamId

getStream' :: Context -> FrameTypeId -> StreamId -> Maybe Stream -> IO (Maybe Stream)
getStream' ctx@Context{..} ftyp _streamId js@(Just strm0) = do
    when (ftyp == FrameHeaders) $ do
        st <- readStreamState strm0
        when (isHalfClosedRemote st) $ E.throwIO $ ConnectionError StreamClosed "header must not be sent to half or fully closed stream"
        -- Priority made an idle stream
        when (isIdle st) $ opened ctx strm0
    return js
getStream' ctx@Context{..} ftyp streamId Nothing
  | isServerInitiated streamId = return Nothing
  | isServer ctx = do
        csid <- getPeerStreamID ctx
        if streamId <= csid then -- consider the stream closed
          if ftyp `elem` [FrameWindowUpdate, FrameRSTStream, FramePriority] then
              return Nothing -- will be ignored
            else
              E.throwIO $ ConnectionError ProtocolError "stream identifier must not decrease"
          else do -- consider the stream idle
            when (ftyp `notElem` [FrameHeaders,FramePriority]) $
                E.throwIO $ ConnectionError ProtocolError $ "this frame is not allowed in an idle stream: " `BS.append` C8.pack (show ftyp)
            when (ftyp == FrameHeaders) $ do
                setPeerStreamID ctx streamId
                cnt <- readIORef concurrency
                -- Checking the limitation of concurrency
                when (cnt >= maxConcurrency) $ E.throwIO $ StreamError RefusedStream streamId
            Just <$> createStream ctx streamId ftyp
  | otherwise = undefined -- never reach

----------------------------------------------------------------

control :: FrameTypeId -> FrameHeader -> ByteString -> Context -> IO Bool
control FrameSettings header@FrameHeader{flags} bs Context{http2settings, controlQ, firstSettings, streamTable} = do
    SettingsFrame alist <- guardIt $ decodeSettingsFrame header bs
    traverse_ E.throwIO $ checkSettingsList alist
    -- HTTP/2 Setting from a browser
    unless (testAck flags) $ do
        oldws <- initialWindowSize <$> readIORef http2settings
        modifyIORef' http2settings $ \old -> updateSettings old alist
        newws <- initialWindowSize <$> readIORef http2settings
        let diff = newws - oldws
        when (diff /= 0) $ updateAllStreamWindow (+ diff) streamTable
        let frame = settingsFrame setAck []
        sent <- readIORef firstSettings
        let setframe
              | sent      = CSettings               frame alist
              | otherwise = CSettings0 initialFrame frame alist
        unless sent $ writeIORef firstSettings True
        enqueueControl controlQ setframe
    return True

control FramePing FrameHeader{flags} bs Context{controlQ} =
    if testAck flags then
        return True -- just ignore
      else do
        let frame = pingFrame bs
        enqueueControl controlQ $ CFrame frame
        return True

control FrameGoAway _ _ Context{controlQ} = do
    enqueueControl controlQ CFinish
    return False

control FrameWindowUpdate header bs Context{connectionWindow} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- atomically $ do
      w0 <- readTVar connectionWindow
      let w1 = w0 + n
      writeTVar connectionWindow w1
      return w1
    when (isWindowOverflow w) $ E.throwIO $ ConnectionError FlowControlError "control window should be less than 2^31"
    return True

control _ _ _ _ =
    -- must not reach here
    return False

----------------------------------------------------------------

{-# INLINE guardIt #-}
guardIt :: Either HTTP2Error a -> IO a
guardIt x = case x of
    Left err    -> E.throwIO err
    Right frame -> return frame


{-# INLINE checkPriority #-}
checkPriority :: Priority -> StreamId -> IO ()
checkPriority p me
  | dep == me = E.throwIO $ StreamError ProtocolError me
  | otherwise = return ()
  where
    dep = streamDependency p

stream :: FrameTypeId -> FrameHeader -> ByteString -> Context -> StreamState -> Stream -> IO StreamState
stream FrameHeaders header@FrameHeader{flags} bs ctx (Open JustOpened) Stream{streamNumber} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    pri <- case mp of
        Nothing -> return defaultPriority
        Just p  -> do
            checkPriority p streamNumber
            return p
    let endOfStream = testEndStream flags
        endOfHeader = testEndHeader flags
    if endOfHeader then do
        tbl <- hpackDecodeHeader frag ctx -- fixme
        return $ if endOfStream then
                    Open (NoBody tbl pri)
                   else
                    Open (HasBody tbl pri)
      else do
        let siz = BS.length frag
        return $ Open $ Continued [frag] siz 1 endOfStream pri

stream FrameHeaders header@FrameHeader{flags} bs ctx (Open (Body q _ _ tlr)) _ = do
    HeadersFrame _ frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
    if endOfStream then do
        tbl <- hpackDecodeTrailer frag ctx
        writeIORef tlr (Just tbl)
        atomically $ writeTQueue q ""
        return HalfClosedRemote
      else
        -- we don't support continuation here.
        E.throwIO $ ConnectionError ProtocolError "continuation in trailer is not supported"

-- ignore data-frame except for flow-control when we're done locally
stream FrameData
       FrameHeader{flags,payloadLength}
       _bs
       Context{controlQ} s@(HalfClosedLocal _)
       Stream{streamNumber} = do
    let endOfStream = testEndStream flags
    when (payloadLength /= 0) $ do
        let frame1 = windowUpdateFrame 0 payloadLength
            frame2 = windowUpdateFrame streamNumber payloadLength
            frame = frame1 `BS.append` frame2
        enqueueControl controlQ $ CFrame frame
    if endOfStream then do
        return HalfClosedRemote
      else
        return s

stream FrameData
       header@FrameHeader{flags,payloadLength,streamId}
       bs
       Context{controlQ} s@(Open (Body q mcl bodyLength _))
       Stream{streamNumber} = do
    DataFrame body <- guardIt $ decodeDataFrame header bs
    let endOfStream = testEndStream flags
    len0 <- readIORef bodyLength
    let len = len0 + payloadLength
    writeIORef bodyLength len
    when (payloadLength /= 0) $ do
        let frame1 = windowUpdateFrame 0 payloadLength
            frame2 = windowUpdateFrame streamNumber payloadLength
            frame = frame1 `BS.append` frame2
        enqueueControl controlQ $ CFrame frame
    atomically $ writeTQueue q body
    if endOfStream then do
        case mcl of
            Nothing -> return ()
            Just cl -> when (cl /= len) $ E.throwIO $ StreamError ProtocolError streamId
        -- no trailers
        atomically $ writeTQueue q ""
        return HalfClosedRemote
      else
        return s

stream FrameContinuation FrameHeader{flags} frag ctx (Open (Continued rfrags siz n endOfStream pri)) _ = do
    let endOfHeader = testEndHeader flags
        rfrags' = frag : rfrags
        siz' = siz + BS.length frag
        n' = n + 1
    when (siz' > 51200) $ -- fixme: hard coding: 50K
      E.throwIO $ ConnectionError EnhanceYourCalm "Header is too big"
    when (n' > 10) $ -- fixme: hard coding
      E.throwIO $ ConnectionError EnhanceYourCalm "Header is too fragmented"
    if endOfHeader then do
        let hdrblk = BS.concat $ reverse rfrags'
        tbl <- hpackDecodeHeader hdrblk ctx
        return $ if endOfStream then
                    Open (NoBody tbl pri)
                   else
                    Open (HasBody tbl pri)
      else
        return $ Open $ Continued rfrags' siz' n' endOfStream pri

stream FrameWindowUpdate header@FrameHeader{streamId} bs _ s Stream{streamWindow} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- atomically $ do
      w0 <- readTVar streamWindow
      let w1 = w0 + n
      writeTVar streamWindow w1
      return w1
    when (isWindowOverflow w) $ E.throwIO $ StreamError FlowControlError streamId
    return s

stream FrameRSTStream header bs ctx _ strm = do
    RSTStreamFrame e <- guardIt $ decoderstStreamFrame header bs
    let cc = Reset e
    closed ctx strm cc
    return $ Closed cc -- will be written to streamState again

stream FramePriority header bs Context{outputQ,priorityTreeSize} s Stream{streamNumber,streamPrecedence} = do
    PriorityFrame newpri <- guardIt $ decodePriorityFrame header bs
    checkPriority newpri streamNumber
    oldpre <- readIORef streamPrecedence
    let newpre = toPrecedence newpri
    writeIORef streamPrecedence newpre
    if isIdle s then do
        n <- atomicModifyIORef' priorityTreeSize (\x -> (x+1,x+1))
        -- fixme hard coding
        when (n >= 20) $ E.throwIO $ ConnectionError EnhanceYourCalm "too many idle priority frames"
        prepare outputQ streamNumber newpri
      else do
        mout <- delete outputQ streamNumber oldpre
        traverse_ (enqueueOutput outputQ) mout
    return s

-- this ordering is important
stream FrameContinuation _ _ _ _ _ = E.throwIO $ ConnectionError ProtocolError "continue frame cannot come here"
stream _ _ _ _ (Open Continued{}) _ = E.throwIO $ ConnectionError ProtocolError "an illegal frame follows header/continuation frames"
-- Ignore frames to streams we have just reset, per section 5.1.
stream _ _ _ _ st@(Closed (ResetByMe _)) _ = return st
stream FrameData FrameHeader{streamId} _ _ _ _ = E.throwIO $ StreamError StreamClosed streamId
stream _ FrameHeader{streamId} _ _ _ _ = E.throwIO $ StreamError ProtocolError streamId

----------------------------------------------------------------

{-# INLINE newReadBody #-}
newReadBody :: TQueue ByteString -> IO (IO ByteString)
newReadBody q = do
    ref <- newIORef False
    return $ readBody q ref

{-# INLINE readBody #-}
readBody :: TQueue ByteString -> IORef Bool -> IO ByteString
readBody q ref = do
    eof <- readIORef ref
    if eof then
        return ""
      else do
        bs <- atomically $ readTQueue q
        when (bs == "") $ writeIORef ref True
        return bs

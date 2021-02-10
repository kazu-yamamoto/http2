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
import Network.HTTP2.Arch.Rate
import Network.HTTP2.Arch.Stream
import Network.HTTP2.Arch.Types
import Network.HTTP2.Frame
import Network.HTTP2.Priority (toPrecedence, delete, prepare)

----------------------------------------------------------------

maxConcurrency :: Int
maxConcurrency = recommendedConcurrency

continuationLimit :: Int
continuationLimit = 10

headerFragmentLimit :: Int
headerFragmentLimit = 51200 -- 50K

idlePriorityLimit :: Int
idlePriorityLimit = 20

pingRateLimit :: Int
pingRateLimit = 4

settingsRateLimit :: Int
settingsRateLimit = 4

emptyFrameRateLimit :: Int
emptyFrameRateLimit = 4

----------------------------------------------------------------

initialFrame :: ByteString
initialFrame = settingsFrame id [(SettingsMaxConcurrentStreams,maxConcurrency)]

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
processFrame ctx _recvN (fid, FrameHeader{streamId})
  | isServer ctx &&
    isServerInitiated streamId &&
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
processFrame ctx recvN (FramePushPromise, header@FrameHeader{payloadLength})
  | isServer ctx = E.throwIO $ ConnectionError ProtocolError "push promise is not allowed"
  | otherwise = do
      pl <- recvN payloadLength
      PushPromiseFrame sid frag <- guardIt $ decodePushPromiseFrame header pl
      unless (isServerInitiated sid) $
          E.throwIO $ ConnectionError ProtocolError "wrong sid for push promise"
      when (frag == "") $
          E.throwIO $ ConnectionError ProtocolError "wrong header fragment for push promise"
      (_,vt) <- hpackDecodeHeader frag ctx
      let ClientInfo{..} = roleInfo ctx
      when (getHeaderValue tokenAuthority vt == Just authority
         && getHeaderValue tokenScheme    vt == Just scheme) $ do
          let mmethod = getHeaderValue tokenMethod vt
              mpath   = getHeaderValue tokenPath   vt
          case (mmethod, mpath) of
            (Just method, Just path) -> do
                strm <- openStream ctx sid FramePushPromise
                insertCache method path strm $ roleInfo ctx
            _ -> return ()
      return True
processFrame ctx@Context{..} recvN typhdr@(ftyp, header@FrameHeader{payloadLength}) = do
    settings <- readIORef http2settings
    case checkFrameHeader settings typhdr of
      Left h2err -> case h2err of
          StreamError err sid -> do
              resetStream err sid
              void $ recvN payloadLength
              return True
          connErr -> E.throwIO connErr
      Right _ -> do
          ex <- E.try $ controlOrStream ctx recvN ftyp header
          case ex of
              Left (StreamError err sid) -> do
                  resetStream err sid
                  return True
              Left connErr -> E.throw connErr
              Right cont -> return cont
  where
    resetStream err sid = do
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
    if isServer ctx then
        atomically $ writeTQueue (inputQ roleInfo) $ Input strm inpObj
      else
        putMVar streamInput inpObj
    return False
processState (Open (HasBody tbl@(_,reqvt) pri)) ctx@Context{..} strm@Stream{streamPrecedence,streamInput} streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    writeIORef streamPrecedence $ toPrecedence pri
    bodyLength <- newIORef 0
    tlr <- newIORef Nothing
    q <- newTQueueIO
    setStreamState ctx strm $ Open (Body q mcl bodyLength tlr)
    bodySource <- mkSource (updateWindow controlQ streamId) q
    let inpObj = InpObj tbl mcl (readSource bodySource) tlr
    if isServer ctx then
        atomically $ writeTQueue (inputQ roleInfo) $ Input strm inpObj
      else
        putMVar streamInput inpObj
    return False
processState s@(Open Continued{}) ctx strm _streamId = do
    setStreamState ctx strm s
    return True
processState HalfClosedRemote ctx strm _streamId = do
    halfClosedRemote ctx strm
    return False
processState s ctx strm _streamId = do
    -- Idle, Open Body, Closed
    setStreamState ctx strm s
    return False

----------------------------------------------------------------

getStream :: Context -> FrameTypeId -> StreamId -> IO (Maybe Stream)
getStream ctx@Context{..} ftyp streamId =
    search streamTable streamId >>= getStream' ctx ftyp streamId

getStream' :: Context -> FrameTypeId -> StreamId -> Maybe Stream -> IO (Maybe Stream)
getStream' ctx ftyp _streamId js@(Just strm0) = do
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
            Just <$> openStream ctx streamId ftyp
  | otherwise = undefined -- never reach

----------------------------------------------------------------

control :: FrameTypeId -> FrameHeader -> ByteString -> Context -> IO Bool
control FrameSettings header@FrameHeader{flags} bs Context{http2settings, controlQ, firstSettings, streamTable, settingsRate} = do
    SettingsFrame alist <- guardIt $ decodeSettingsFrame header bs
    traverse_ E.throwIO $ checkSettingsList alist
    -- HTTP/2 Setting from a browser
    if testAck flags then
        return True
      else do
        -- Settings Flood - CVE-2019-9515
        rate <- getRate settingsRate
        if rate > settingsRateLimit then
            E.throwIO $ ConnectionError ProtocolError "too many settings"
          else do
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

control FramePing FrameHeader{flags} bs Context{controlQ,pingRate} =
    if testAck flags then
        return True
      else do
        -- Ping Flood - CVE-2019-9512
        rate <- getRate pingRate
        if rate > pingRateLimit then
            E.throwIO $ ConnectionError ProtocolError "too many ping"
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
stream FrameHeaders header@FrameHeader{flags} bs ctx s@(Open JustOpened) Stream{streamNumber} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
        endOfHeader = testEndHeader flags
    if frag == "" && not endOfStream && not endOfHeader then do
        -- Empty Frame Flooding - CVE-2019-9518
        rate <- getRate $ emptyFrameRate ctx
        if rate > emptyFrameRateLimit then
            E.throwIO $ ConnectionError ProtocolError "too many empty headers"
          else
            return s
      else do
        pri <- case mp of
            Nothing -> return defaultPriority
            Just p  -> do
                checkPriority p streamNumber
                return p
        if endOfHeader then do
            tbl <- hpackDecodeHeader frag ctx
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
    -- checking frag == "" is not necessary
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
       _ = do
    when (payloadLength /= 0) $ do
        let frame = windowUpdateFrame 0 payloadLength
        enqueueControl controlQ $ CFrame frame
    let endOfStream = testEndStream flags
    if endOfStream then do
        return HalfClosedRemote
      else
        return s

stream FrameData
       header@FrameHeader{flags,payloadLength,streamId}
       bs
       Context{emptyFrameRate} s@(Open (Body q mcl bodyLength _))
       _ = do
    DataFrame body <- guardIt $ decodeDataFrame header bs
    len0 <- readIORef bodyLength
    let len = len0 + payloadLength
        endOfStream = testEndStream flags
    -- Empty Frame Flooding - CVE-2019-9518
    if body == "" then
        unless endOfStream $ do
            rate <- getRate emptyFrameRate
            when (rate > emptyFrameRateLimit) $ do
                E.throwIO $ ConnectionError ProtocolError "too many empty data"
      else do
        writeIORef bodyLength len
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

stream FrameContinuation FrameHeader{flags} frag ctx s@(Open (Continued rfrags siz n endOfStream pri)) _ = do
    let endOfHeader = testEndHeader flags
    if frag == "" && not endOfHeader then do
        -- Empty Frame Flooding - CVE-2019-9518
        rate <- getRate $ emptyFrameRate ctx
        if rate > emptyFrameRateLimit then
            E.throwIO $ ConnectionError ProtocolError "too many empty continuation"
          else
            return s
      else do
        let rfrags' = frag : rfrags
            siz' = siz + BS.length frag
            n' = n + 1
        when (siz' > headerFragmentLimit) $
          E.throwIO $ ConnectionError EnhanceYourCalm "Header is too big"
        when (n' > continuationLimit) $
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
        when (n > idlePriorityLimit) $
            E.throwIO $ ConnectionError EnhanceYourCalm "too many idle priority frames"
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

-- | Type for input streaming.
data Source = Source (Int -> IO ())
                     (TQueue ByteString)
                     (IORef ByteString)
                     (IORef Bool)

mkSource :: (Int -> IO ()) -> TQueue ByteString -> IO Source
mkSource update q = Source update q <$> newIORef "" <*> newIORef False

updateWindow :: TQueue Control -> StreamId -> Int -> IO ()
updateWindow _        _   0   = return ()
updateWindow controlQ sid len = enqueueControl controlQ $ CFrame frame
  where
    frame1 = windowUpdateFrame 0 len
    frame2 = windowUpdateFrame sid len
    frame = frame1 `BS.append` frame2

readSource :: Source -> IO ByteString
readSource (Source update q refBS refEOF) = do
    eof <- readIORef refEOF
    if eof then
        return ""
      else do
        bs <- readBS
        let len = BS.length bs
        update len
        return bs
  where
    readBS = do
        bs0 <- readIORef refBS
        if bs0 == "" then do
            bs <- atomically $ readTQueue q
            when (bs == "") $ writeIORef refEOF True
            return bs
          else do
            writeIORef refBS ""
            return bs0

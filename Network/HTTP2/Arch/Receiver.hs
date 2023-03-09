{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Arch.Receiver (
    frameReceiver
  , maxConcurrency
  , initialFrames
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
import Network.HTTP2.Frame

----------------------------------------------------------------

maxConcurrency :: Int
maxConcurrency = recommendedConcurrency

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

----------------------------------------------------------------

initialFrames :: Config -> [ByteString]
initialFrames Config{..} = [frame1,frame2]
  where
    len = confBufferSize - frameHeaderLength
    payloadLength = max defaultPayloadLength len
    alist = [(SettingsMaxFrameSize,payloadLength)
            ,(SettingsMaxConcurrentStreams,maxConcurrency)
            ,(SettingsInitialWindowSize,maxWindowSize)]
    frame1 = settingsFrame id alist
    frame2 = windowUpdateFrame 0 (maxWindowSize - defaultWindowSize)

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
        if BS.null hd then
            enqueueControl controlQ $ CFinish ConnectionIsClosed
          else do
            processFrame ctx conf $ decodeFrameHeader hd
            loop (n + 1)

    sendGoaway se
      | Just e@ConnectionIsClosed  <- E.fromException se =
          enqueueControl controlQ $ CFinish e
      | Just e@(ConnectionErrorIsReceived _ _ _) <- E.fromException se =
          enqueueControl controlQ $ CFinish e
      | Just e@(ConnectionErrorIsSent err sid msg) <- E.fromException se = do
          let frame = goawayFrame sid err $ Short.fromShort msg
          enqueueControl controlQ $ CFrames Nothing [frame]
          enqueueControl controlQ $ CFinish e
      | Just e@(StreamErrorIsSent err sid) <- E.fromException se = do
          let frame = resetFrame err sid
          enqueueControl controlQ $ CFrames Nothing [frame]
          let frame' = goawayFrame sid err "treat a stream error as a connection error"
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
  | isServer ctx &&
    isServerInitiated streamId &&
    (fid `notElem` [FramePriority,FrameRSTStream,FrameWindowUpdate]) =
    E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "stream id should be odd"

processFrame Context{..} Config{..} (ftyp, FrameHeader{payloadLength,streamId})
  | ftyp > maxFrameType = do
    mx <- readIORef continued
    case mx of
        Nothing -> do
            -- ignoring unknown frame
            void $ confReadN payloadLength
        Just _  -> E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "unknown frame"
processFrame ctx Config{..} (FramePushPromise, header@FrameHeader{payloadLength,streamId})
  | isServer ctx = E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "push promise is not allowed"
  | otherwise = do
      pl <- confReadN payloadLength
      PushPromiseFrame sid frag <- guardIt $ decodePushPromiseFrame header pl
      unless (isServerInitiated sid) $
          E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "wrong sid for push promise"
      when (frag == "") $
          E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "wrong header fragment for push promise"
      (_,vt) <- hpackDecodeHeader frag streamId ctx
      let ClientInfo{..} = toClientInfo $ roleInfo ctx
      when (getHeaderValue tokenAuthority vt == Just authority
         && getHeaderValue tokenScheme    vt == Just scheme) $ do
          let mmethod = getHeaderValue tokenMethod vt
              mpath   = getHeaderValue tokenPath   vt
          case (mmethod, mpath) of
            (Just method, Just path) -> do
                strm <- openStream ctx sid FramePushPromise
                insertCache method path strm $ roleInfo ctx
            _ -> return ()
processFrame ctx@Context{..} conf typhdr@(ftyp, header) = do
    settings <- readIORef peerSettings
    case checkFrameHeader settings typhdr of
      Left (FrameDecodeError ec sid msg) -> E.throwIO $ ConnectionErrorIsSent ec sid msg
      Right _    -> controlOrStream ctx conf ftyp header

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
    setContinued   = writeIORef continued $ Just streamId
    resetContinued = writeIORef continued Nothing
    checkContinued = do
        mx <- readIORef continued
        case mx of
            Nothing  -> return ()
            Just sid
              | sid == streamId && ftyp == FrameContinuation -> return ()
              | otherwise -> E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "continuation frame must follow"

----------------------------------------------------------------

processState :: StreamState -> Context -> Stream -> StreamId -> IO Bool
processState (Open (NoBody tbl@(_,reqvt))) ctx@Context{..} strm@Stream{streamInput} streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    when (just mcl (/= (0 :: Int))) $ E.throwIO $ StreamErrorIsSent ProtocolError streamId
    halfClosedRemote ctx strm
    tlr <- newIORef Nothing
    let inpObj = InpObj tbl (Just 0) (return "") tlr
    if isServer ctx then do
        let si = toServerInfo roleInfo
        atomically $ writeTQueue (inputQ si) $ Input strm inpObj
      else
        putMVar streamInput inpObj
    return False
processState (Open (HasBody tbl@(_,reqvt))) ctx@Context{..} strm@Stream{streamInput} streamId = do
    let mcl = fst <$> (getHeaderValue tokenContentLength reqvt >>= C8.readInt)
    bodyLength <- newIORef 0
    tlr <- newIORef Nothing
    q <- newTQueueIO
    setStreamState ctx strm $ Open (Body q mcl bodyLength tlr)
    incref <- newIORef 0
    bodySource <- mkSource q $ updateWindow controlQ streamId incref
    let inpObj = InpObj tbl mcl (readSource bodySource) tlr
    if isServer ctx then do
        let si = toServerInfo roleInfo
        atomically $ writeTQueue (inputQ si) $ Input strm inpObj
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

getStream :: Context -> FrameType -> StreamId -> IO (Maybe Stream)
getStream ctx@Context{..} ftyp streamId =
    search streamTable streamId >>= getStream' ctx ftyp streamId

getStream' :: Context -> FrameType -> StreamId -> Maybe Stream -> IO (Maybe Stream)
getStream' ctx ftyp streamId js@(Just strm0) = do
    when (ftyp == FrameHeaders) $ do
        st <- readStreamState strm0
        when (isHalfClosedRemote st) $ E.throwIO $ ConnectionErrorIsSent StreamClosed streamId "header must not be sent to half or fully closed stream"
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
              E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "stream identifier must not decrease"
          else do -- consider the stream idle
            when (ftyp `notElem` [FrameHeaders,FramePriority]) $ do
                let errmsg = Short.toShort ("this frame is not allowed in an idle stream: " `BS.append` (C8.pack (show ftyp)))
                E.throwIO $ ConnectionErrorIsSent ProtocolError streamId errmsg
            when (ftyp == FrameHeaders) $ do
                setPeerStreamID ctx streamId
                cnt <- readIORef concurrency
                -- Checking the limitation of concurrency
                when (cnt >= maxConcurrency) $ E.throwIO $ StreamErrorIsSent RefusedStream streamId
            Just <$> openStream ctx streamId ftyp
  | otherwise = undefined -- never reach

----------------------------------------------------------------

type Payload = ByteString

control :: FrameType -> FrameHeader -> Payload -> Context -> Config -> IO ()
control FrameSettings header@FrameHeader{flags,streamId} bs Context{peerSettings, controlQ, firstSettings, streamTable, settingsRate} conf = do
    SettingsFrame peerAlist <- guardIt $ decodeSettingsFrame header bs
    traverse_ E.throwIO $ checkSettingsList peerAlist
    -- HTTP/2 Setting from a browser
    unless (testAck flags) $ do
        -- Settings Flood - CVE-2019-9515
        rate <- getRate settingsRate
        if rate > settingsRateLimit then
            E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many settings"
          else do
            oldws <- initialWindowSize <$> readIORef peerSettings
            modifyIORef' peerSettings $ \old -> updateSettings old peerAlist
            newws <- initialWindowSize <$> readIORef peerSettings
            let diff = newws - oldws
            when (diff /= 0) $ updateAllStreamWindow (+ diff) streamTable
            let frame = settingsFrame setAck []
            sent <- readIORef firstSettings
            let setframe
                  | sent      = CFrames (Just peerAlist) [frame]
                  -- server side
                  | otherwise = CFrames (Just peerAlist) (initialFrames conf ++ [frame])
            unless sent $ writeIORef firstSettings True
            enqueueControl controlQ setframe

control FramePing FrameHeader{flags,streamId} bs Context{controlQ,pingRate} _ =
    unless (testAck flags) $ do
        -- Ping Flood - CVE-2019-9512
        rate <- getRate pingRate
        if rate > pingRateLimit then
            E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many ping"
          else do
            let frame = pingFrame bs
            enqueueControl controlQ $ CFrames Nothing [frame]

control FrameGoAway header bs _ _ = do
    GoAwayFrame sid err msg <- guardIt $ decodeGoAwayFrame header bs
    if err == NoError then
        E.throwIO ConnectionIsClosed
      else
        E.throwIO $ ConnectionErrorIsReceived err sid $ Short.toShort msg

control FrameWindowUpdate header@FrameHeader{streamId} bs Context{connectionWindow} _ = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- atomically $ do
      w0 <- readTVar connectionWindow
      let w1 = w0 + n
      writeTVar connectionWindow w1
      return w1
    when (isWindowOverflow w) $ E.throwIO $ ConnectionErrorIsSent FlowControlError streamId "control window should be less than 2^31"

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
  | dep == me = E.throwIO $ StreamErrorIsSent ProtocolError me
  | otherwise = return ()
  where
    dep = streamDependency p

stream :: FrameType -> FrameHeader -> ByteString -> Context -> StreamState -> Stream -> IO StreamState
stream FrameHeaders header@FrameHeader{flags,streamId} bs ctx s@(Open JustOpened) Stream{streamNumber} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
        endOfHeader = testEndHeader flags
    if frag == "" && not endOfStream && not endOfHeader then do
        -- Empty Frame Flooding - CVE-2019-9518
        rate <- getRate $ emptyFrameRate ctx
        if rate > emptyFrameRateLimit then
            E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many empty headers"
          else
            return s
      else do
        case mp of
          Nothing -> return ()
          Just p  -> checkPriority p streamNumber
        if endOfHeader then do
            tbl <- hpackDecodeHeader frag streamId ctx
            return $ if endOfStream then
                        Open (NoBody tbl)
                       else
                        Open (HasBody tbl)
          else do
            let siz = BS.length frag
            return $ Open $ Continued [frag] siz 1 endOfStream

stream FrameHeaders header@FrameHeader{flags,streamId} bs ctx (Open (Body q _ _ tlr)) _ = do
    HeadersFrame _ frag <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
    -- checking frag == "" is not necessary
    if endOfStream then do
        tbl <- hpackDecodeTrailer frag streamId ctx
        writeIORef tlr (Just tbl)
        atomically $ writeTQueue q ""
        return HalfClosedRemote
      else
        -- we don't support continuation here.
        E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "continuation in trailer is not supported"

-- ignore data-frame except for flow-control when we're done locally
stream FrameData
       FrameHeader{flags,payloadLength}
       _bs
       ctx s@(HalfClosedLocal _)
       _ = do
    connectionWindowIncrement ctx payloadLength
    let endOfStream = testEndStream flags
    if endOfStream then do
        return HalfClosedRemote
      else
        return s

stream FrameData
       header@FrameHeader{flags,payloadLength,streamId}
       bs
       ctx@Context{emptyFrameRate} s@(Open (Body q mcl bodyLength _))
       _ = do
    connectionWindowIncrement ctx payloadLength
    DataFrame body <- guardIt $ decodeDataFrame header bs
    len0 <- readIORef bodyLength
    let len = len0 + payloadLength
        endOfStream = testEndStream flags
    -- Empty Frame Flooding - CVE-2019-9518
    if body == "" then
        unless endOfStream $ do
            rate <- getRate emptyFrameRate
            when (rate > emptyFrameRateLimit) $ do
                E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many empty data"
      else do
        writeIORef bodyLength len
        atomically $ writeTQueue q body
    if endOfStream then do
        case mcl of
            Nothing -> return ()
            Just cl -> when (cl /= len) $ E.throwIO $ StreamErrorIsSent ProtocolError streamId
        -- no trailers
        atomically $ writeTQueue q ""
        return HalfClosedRemote
      else
        return s

stream FrameContinuation FrameHeader{flags,streamId} frag ctx s@(Open (Continued rfrags siz n endOfStream)) _ = do
    let endOfHeader = testEndHeader flags
    if frag == "" && not endOfHeader then do
        -- Empty Frame Flooding - CVE-2019-9518
        rate <- getRate $ emptyFrameRate ctx
        if rate > emptyFrameRateLimit then
            E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "too many empty continuation"
          else
            return s
      else do
        let rfrags' = frag : rfrags
            siz' = siz + BS.length frag
            n' = n + 1
        when (siz' > headerFragmentLimit) $
          E.throwIO $ ConnectionErrorIsSent EnhanceYourCalm streamId "Header is too big"
        when (n' > continuationLimit) $
          E.throwIO $ ConnectionErrorIsSent EnhanceYourCalm streamId "Header is too fragmented"
        if endOfHeader then do
            let hdrblk = BS.concat $ reverse rfrags'
            tbl <- hpackDecodeHeader hdrblk streamId ctx
            return $ if endOfStream then
                        Open (NoBody tbl)
                       else
                        Open (HasBody tbl)
          else
            return $ Open $ Continued rfrags' siz' n' endOfStream

stream FrameWindowUpdate header@FrameHeader{streamId} bs _ s Stream{streamWindow} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- atomically $ do
      w0 <- readTVar streamWindow
      let w1 = w0 + n
      writeTVar streamWindow w1
      return w1
    when (isWindowOverflow w) $ E.throwIO $ StreamErrorIsSent FlowControlError streamId
    return s

stream FrameRSTStream header@FrameHeader{streamId} bs ctx _ strm = do
    RSTStreamFrame err <- guardIt $ decodeRSTStreamFrame header bs
    let cc = Reset err
    closed ctx strm cc
    E.throwIO $ StreamErrorIsReceived err streamId

stream FramePriority header bs _ s Stream{streamNumber} = do
    -- ignore
    -- Resource Loop - CVE-2019-9513
    PriorityFrame newpri <- guardIt $ decodePriorityFrame header bs
    checkPriority newpri streamNumber
    return s

-- this ordering is important
stream FrameContinuation FrameHeader{streamId} _ _ _ _ = E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "continue frame cannot come here"
stream _ FrameHeader{streamId} _ _ (Open Continued{}) _ = E.throwIO $ ConnectionErrorIsSent ProtocolError streamId "an illegal frame follows header/continuation frames"
-- Ignore frames to streams we have just reset, per section 5.1.
stream _ _ _ _ st@(Closed (ResetByMe _)) _ = return st
stream FrameData FrameHeader{streamId} _ _ _ _ = E.throwIO $ StreamErrorIsSent StreamClosed streamId
stream _ FrameHeader{streamId} _ _ _ _ = E.throwIO $ StreamErrorIsSent ProtocolError streamId

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source (Int -> IO ())
                     (TQueue ByteString)
                     (IORef ByteString)
                     (IORef Bool)

mkSource :: TQueue ByteString -> (Int -> IO ()) -> IO Source
mkSource q update = Source update q <$> newIORef "" <*> newIORef False

updateWindow :: TQueue Control -> StreamId -> IORef Int -> Int -> IO ()
updateWindow _ _        _   0   = return ()
updateWindow controlQ sid incref len = do
    w0 <- readIORef incref
    let w1 = w0 + len
    if w1 >= defaultWindowSize then do -- fixme
        let frame = windowUpdateFrame sid w1
        enqueueControl controlQ $ CFrames Nothing [frame]
        writeIORef incref 0
      else
        writeIORef incref w1

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

----------------------------------------------------------------

connectionWindowIncrement :: Context -> Int -> IO ()
connectionWindowIncrement Context{..} len = do
    w0 <- readIORef connectionInc
    let w1 = w0 + len
    if w1 >= defaultWindowSize then do -- fixme
        let frame = windowUpdateFrame 0 w1
        enqueueControl controlQ $ CFrames Nothing [frame]
        writeIORef connectionInc 0
      else
        writeIORef connectionInc w1

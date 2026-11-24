{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Network.HTTP2.H2.Types where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IORef
import Foreign.Ptr (nullPtr)
import Network.Control
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal (RequestGivenUp (..))
import qualified Network.HTTP.Semantics.Error as HE
import Network.HTTP.Semantics.IO
import Network.Socket hiding (Stream)
import System.IO.Unsafe
import qualified System.TimeManager as T
import qualified System.TimeManager as TM

import Imports
import Network.HPACK
import Network.HTTP2.Frame

----------------------------------------------------------------

{-

== Stream state

The stream state is stored in the 'streamState' field (an @IORef@) of a
'Stream'. The main place where the stream state is updated is in
'controlOrStream', which does something like this:

> state0 <- readStreamState strm
> state1 <- stream .. state0 ..
> processState .. state1 ..

where 'processState' updates the @IORef@, based on 'state1' (the state computed
by 'stream') and the /current/ state of the stream; for simplicity, we will
assume here that this must equal 'state0' (it might not, if a concurrent thread
changed the stream state).

The diagram below summarizes the stream state transitions on the client side,
omitting error cases (which result in exceptions being thrown). Each transition
is labelled with the relevant case in either the function 'stream' or the
function 'processState'.

>               [Open JustOpened]
>                      |
>                      |
>              HEADERS CONTINUATION*
>                      |
>                      | (stream1)
>                      |
>                 END_STREAM?
>                      |
>            _________/ \_________
>           /      yes   no       \
>           |                     |
>      [Open NoBody]        [Open HasBody]
>           |                     |
>           | (process1)          | (process2)
>           |                     |
>  [HalfClosedRemote] <--\   [Open Body] <----------------------\
>           |             |        |                             |
>           |             |        +---------------\             |
>       RST_STREAM        |        |               |             |
>           |             |     HEADERS CONT*     DATA           |
>           | (stream6)   |        |               |             |
>           |             |        | (stream2)     | (stream4)   |
>           | (process5)  |        |               |             |
>           |             |   END_STREAM?      END_STREAM?       |
>        [Closed]         |        |               |             |
>                         |        | yes      yes / \ no         |
>                         \--------+-------------/   \-----------/
>                          (process4)                 (process6)

Notes:

- The 'HalfClosedLocal' state is not used on the client side.
- Indeed, unless an exception is thrown, even the 'Closed' stream state is not
  used in the client; when the @IORef@ is collected, it is typically in
  'HalfClosedRemote' state.

-}

data OpenState
    = JustOpened
    | NoBody TokenHeaderTable
    | HasBody TokenHeaderTable
    | Body
        (TQueue (Either E.SomeException (ByteString, Bool)))
        (Maybe Int) -- received Content-Length
        -- compared the body length for error checking
        (IORef Int) -- actual body length
        (IORef (Maybe TokenHeaderTable)) -- trailers

-- | Header block fragments accumulated so far.
--
-- Fragments are stored in reverse order (newest first).
data PartialHeaderBlock = PartialHeaderBlock
    { phbFragments :: [HeaderBlockFragment]
    , phbTotalSize :: Int
    , phbNumFrames :: Int
    }

data ClosedCode
    = Finished
    | -- | Above the last stream identifier of the peer's GOAWAY.
      Unprocessed
    | Killed
    | Reset ErrorCode
    | ResetByMe E.SomeException
    deriving (Show)

-- | Used for streams which are cancelled by calling
-- 'Network.HTTP.Semantics.outBodyCancel'.
data CancelledStream = CancelledStream
    deriving (Show, E.Exception)

-- | What a wait on a closed stream throws: a request given up on says
--   so, to be returned as 'Left' by 'sendRequestWith'.
closedCodeToException :: StreamId -> ClosedCode -> E.SomeException
closedCodeToException _ (ResetByMe e)
    | Just RequestGivenUp <- E.fromException e = e
closedCodeToException sid cc = E.toException $ closedCodeToError sid cc

closedCodeToError :: StreamId -> ClosedCode -> HTTP2Error
closedCodeToError sid cc =
    case cc of
        Finished -> ConnectionIsClosed
        Unprocessed -> RequestIsUnprocessed $ Just sid
        Killed -> ConnectionIsTimeout
        Reset err -> StreamResetIsReceived err sid
        -- A reset we sent for an error of our own kind is told as it is.
        ResetByMe err -> fromMaybe (BadThingHappen err) $ E.fromException err

-- | Whether an exception is an 'HTTP2Error': the stream or the connection
--   ended, by the peer, the connection, or a reset of ours whose reason is
--   told where it arose.  Not the application's failure, so not printed.
isStreamEnd :: E.SomeException -> Bool
isStreamEnd se = isJust (E.fromException se :: Maybe HTTP2Error)

-- | Running a thread's action, letting the end of its stream go quietly.
quietOnEnd :: IO () -> IO ()
quietOnEnd io = io `E.catch` ignore
  where
    ignore :: HTTP2Error -> IO ()
    ignore _ = return ()

----------------------------------------------------------------

data StreamState
    = Idle
    | Open (Maybe ClosedCode) OpenState -- HalfClosedLocal if Just
    | HalfClosedRemote
    | Closed ClosedCode
    | Reserved

instance Show StreamState where
    show Idle = "Idle"
    show (Open Nothing _) = "Open"
    show (Open (Just e) _) = "HalfClosedLocal: " ++ show e
    show HalfClosedRemote = "HalfClosedRemote"
    show (Closed e) = "Closed: " ++ show e
    show Reserved = "Reserved"

----------------------------------------------------------------

type RxQ = TQueue (Either E.SomeException (ByteString, Bool))

data Stream = Stream
    { streamNumber :: StreamId
    , streamState :: TVar StreamState
    , streamInput :: TMVar (Either E.SomeException InpObj) -- Client only
    , streamTxFlow :: TVar TxFlow
    , streamRxFlow :: IORef RxFlow
    , streamRxQ :: IORef (Maybe RxQ)
    , streamRequestMethod :: IORef (Maybe ByteString)
    -- ^ Client only: the method of the request, which decides whether the
    --   response may have content at all (RFC 9110, section 6.4.1)
    , streamDiscardBody :: TVar Bool
    -- ^ Server only: the handler has returned, and nobody will read the
    --   rest of the request body.  It is taken in and thrown away, both
    --   windows given back as it comes, until the peer ends it.
    , streamGiveUp :: IORef (STM ())
    -- ^ Client only: what the request gives up on ('outObjGiveUp').
    }

instance Show Stream where
    show Stream{..} =
        "Stream{id="
            ++ show streamNumber
            ++ ",state="
            ++ show (unsafePerformIO (readTVarIO streamState))
            ++ "}"

----------------------------------------------------------------

data Output = Output
    { outputStream :: Stream
    , outputType :: OutputType
    , outputSync :: Maybe Output -> IO ()
    , outputFail :: E.SomeException -> IO ()
    -- ^ Telling whoever waits on 'outputSync' that the output failed.
    }

data OutputType
    = OHeader [Header] (Maybe DynaNext) TrailersMaker
    | OPush TokenHeaderList StreamId -- associated stream id from client
    | ONext DynaNext TrailersMaker
    | OInformational [Header]
    | OReset (Maybe E.SomeException)

data Sync = Done | Cont Output | Failed E.SomeException

----------------------------------------------------------------

data Control = CFrames (Maybe SettingsList) [ByteString]

-- | Control frames waiting for the sender, and how many there are: see
-- 'Network.HTTP2.H2.Queue.waitControlQ'.
data ControlQ = ControlQ (TQueue Control) (TVar Int)

----------------------------------------------------------------

type ReasonPhrase = ShortByteString

-- | The connection error or the stream error: http-semantics'
--   'HE.HTTPError', the same as HTTP\/3's.  The constructors of the type
--   this used to be remain as pattern synonyms.
--
--   A stream error resets that stream and the connection carries on, as
--   RFC 9113 section 5.4.2 asks. One kind of trouble is a connection error
--   even though the spec calls it a stream error, because this
--   implementation cannot carry on through it: a field block abandoned
--   part-way leaves the HPACK tables disagreeing with the peer's, and
--   nothing sent afterwards would decode.
--   The 'StreamId' of a connection error is not kept: it is 0 when matched.
type HTTP2Error = HE.HTTPError

pattern ConnectionIsClosed :: HTTP2Error
pattern ConnectionIsClosed = HE.ConnectionIsClosed

pattern ConnectionIsTimeout :: HTTP2Error
pattern ConnectionIsTimeout = HE.ConnectionIsTimeout

-- | The peer reset the connection.
pattern ConnectionIsReset :: HTTP2Error
pattern ConnectionIsReset = HE.ConnectionIsReset

-- | The peer will not process this request, after its GOAWAY: it may be
--   sent again.  The stream, if the request had got one.
pattern RequestIsUnprocessed :: Maybe StreamId -> HTTP2Error
pattern RequestIsUnprocessed msid <- HE.RequestIsUnprocessed (fmap fromIntegral -> msid)
    where
        RequestIsUnprocessed msid = HE.RequestIsUnprocessed $ fromIntegral <$> msid

pattern ConnectionErrorIsReceived
    :: ErrorCode -> StreamId -> ReasonPhrase -> HTTP2Error
pattern ConnectionErrorIsReceived e sid msg <-
    (connectionErrorIsReceived -> Just (e, sid, msg))
    where
        ConnectionErrorIsReceived e _ msg = HE.ConnectionErrorIsReceived (toHE e) msg

pattern ConnectionErrorIsSent
    :: ErrorCode -> StreamId -> ReasonPhrase -> HTTP2Error
pattern ConnectionErrorIsSent e sid msg <-
    (connectionErrorIsSent -> Just (e, sid, msg))
    where
        ConnectionErrorIsSent e _ msg = HE.ConnectionErrorIsSent (toHE e) msg

pattern StreamErrorIsReceived :: ErrorCode -> StreamId -> HTTP2Error
pattern StreamErrorIsReceived e sid <-
    HE.StreamErrorIsReceived (fromHE -> e) (fromIntegral -> sid)
    where
        StreamErrorIsReceived e sid = HE.StreamErrorIsReceived (toHE e) (fromIntegral sid)

-- | The same as 'StreamErrorIsReceived'.
pattern StreamResetIsReceived :: ErrorCode -> StreamId -> HTTP2Error
pattern StreamResetIsReceived e sid = StreamErrorIsReceived e sid

pattern StreamErrorIsSent :: ErrorCode -> StreamId -> ReasonPhrase -> HTTP2Error
pattern StreamErrorIsSent e sid msg <-
    HE.StreamErrorIsSent (fromHE -> e) (fromIntegral -> sid) msg
    where
        StreamErrorIsSent e sid msg = HE.StreamErrorIsSent (toHE e) (fromIntegral sid) msg

pattern BadThingHappen :: E.SomeException -> HTTP2Error
pattern BadThingHappen e = HE.BadThingHappen e

{-# COMPLETE
    ConnectionIsClosed
    , ConnectionIsTimeout
    , ConnectionIsReset
    , RequestIsUnprocessed
    , ConnectionErrorIsReceived
    , ConnectionErrorIsSent
    , StreamErrorIsReceived
    , StreamErrorIsSent
    , BadThingHappen
    #-}

connectionErrorIsReceived
    :: HTTP2Error -> Maybe (ErrorCode, StreamId, ReasonPhrase)
connectionErrorIsReceived (HE.ConnectionErrorIsReceived c msg) = Just (fromHE c, 0, msg)
connectionErrorIsReceived _ = Nothing

connectionErrorIsSent :: HTTP2Error -> Maybe (ErrorCode, StreamId, ReasonPhrase)
connectionErrorIsSent (HE.ConnectionErrorIsSent c msg) = Just (fromHE c, 0, msg)
connectionErrorIsSent _ = Nothing

toHE :: ErrorCode -> HE.HTTPErrorCode
toHE = HE.http2ErrorCode . fromIntegral . fromErrorCode

fromHE :: HE.HTTPErrorCode -> ErrorCode
fromHE = toErrorCode . fromIntegral . HE.errorValue

-- | What a client connection throws inside once it gives up
--   ('Network.HTTP2.Client.runGivingUp' returns 'Left').
givenUpConnection :: HTTP2Error
givenUpConnection = ConnectionErrorIsSent NoError 0 "given up"

----------------------------------------------------------------

-- | Checking 'SettingsList' and reporting an error if any.
--
-- >>> checkSettingsList [(SettingsEnablePush,2)]
-- Just (ConnectionErrorIsSent (HTTPErrorCode {errorKind = ProtocolViolation, errorSpace = HTTP2, errorValue = 1}) "enable push must be 0 or 1")
checkSettingsList :: SettingsList -> Maybe HTTP2Error
checkSettingsList settings = case mapMaybe checkSettingsValue settings of
    [] -> Nothing
    (x : _) -> Just x

checkSettingsValue :: (SettingsKey, SettingsValue) -> Maybe HTTP2Error
checkSettingsValue (SettingsEnablePush, v)
    | v /= 0 && v /= 1 =
        Just $ ConnectionErrorIsSent ProtocolError 0 "enable push must be 0 or 1"
checkSettingsValue (SettingsInitialWindowSize, v)
    | v > maxWindowSize =
        Just $
            ConnectionErrorIsSent
                FlowControlError
                0
                "Window size must be less than or equal to 65535"
checkSettingsValue (SettingsMaxFrameSize, v)
    | v < defaultPayloadLength || v > maxPayloadLength =
        Just $
            ConnectionErrorIsSent
                ProtocolError
                0
                "Max frame size must be in between 16384 and 16777215"
checkSettingsValue _ = Nothing

----------------------------------------------------------------

-- | HTTP/2 configuration.
data Config = Config
    { confWriteBuffer :: Buffer
    -- ^ This is used only by frameSender.
    -- This MUST be freed after frameSender is terminated.
    , confBufferSize :: BufferSize
    -- ^ The size of the write buffer.
    --   We assume that the read buffer is the same size.
    --   So, this value is announced via SETTINGS_MAX_FRAME_SIZE
    --   to the peer.
    , confSendAll :: ByteString -> IO ()
    -- ^ Not used when 'confSendAllWith' is 'Just'.  When replacing it in
    --   a config which has one, e.g. from 'allocSimpleConfig', set
    --   'confSendAllWith' to 'Nothing' or to a send of the same kind.
    , confSendAllWith :: Maybe (ByteString -> STM () -> IO (Either () ()))
    -- ^ 'confSendAll' which the library can give up on: 'Left' when the
    --   'STM' action fires first.  With it, a write which the peer does
    --   not read is ended by the timeout rather than by killing a thread.
    --   Part of the data may have gone, so the connection is closed after.
    --
    --   @since 5.4.13
    , confReadN :: Int -> IO ByteString
    -- ^ When 'confReadNWith' is 'Just', frames are read with it and only
    --   the connection preface with this.  When replacing it in a config
    --   which has one, e.g. from 'allocSimpleConfig', set 'confReadNWith'
    --   to 'Nothing' or to a read of the same kind sharing its buffer.
    , confReadNWith :: Maybe (Int -> STM () -> IO (Either () ByteString))
    -- ^ 'confReadN' which the library can give up on: 'Left' when the
    --   'STM' action fires first, and 'Right' with the empty string at end
    --   of input.  With it, the reader is stopped without being interrupted.
    --   It must share its buffer with 'confReadN'.
    --
    --   @since 5.4.13
    , confPositionReadMaker :: PositionReadMaker
    , confTimeoutManager :: TM.Manager
    -- ^ Deprecated field. Not used any more. Timeouts are decided by
    --   'confTimeout'.
    , confMySockAddr :: SockAddr
    -- ^ This is copied into 'Aux', if exist, on server.
    , confPeerSockAddr :: SockAddr
    -- ^ This is copied into 'Aux', if exist, on server.
    , confReadNTimeout :: Bool
    -- ^ Deprecated field. Not used any more. Timeouts are decided by
    --   'confTimeout'.
    , confOnInformational :: StreamId -> TokenHeaderTable -> IO ()
    -- ^ Client only: called when a 1xx informational response (e.g. 103 Early
    --   Hints) is received on the given stream, ahead of the final response.
    --   No-op by default.
    --
    --   @since 5.4.2
    , confTimeout :: Int
    -- ^ How long this connection may make no progress, in microseconds.
    --   Zero or less means no timeout.
    --
    --   The library supervises the connection itself, and gives up on it
    --   with GOAWAY when the time passes. A connection belongs to one
    --   supervisor: a caller which watched the connection before handing
    --   it here, as Warp does, stops watching it when it does.
    --
    --   Progress is exactly this:
    --
    --   * The peer makes progress when the header of a frame arrives,
    --     which also means that the payload of the previous frame was
    --     read in full. Any frame counts, on any stream, including PING,
    --     SETTINGS and WINDOW_UPDATE. So a peer can keep an idle
    --     connection alive with PINGs, as far as the PING rate limit
    --     allows. A frame which trickles in counts only once its header
    --     is complete.
    --
    --   * A write makes progress when a call to 'confSendAll' returns. A
    --     call which is blocked counts for nothing, however many bytes
    --     the kernel took.
    --
    --   A server application runs from when its stream is handed to it
    --   until it returns, and is not limited in time. A stream waits for
    --   its request body while the application reads it, and for the peer
    --   while a window the peer has shut holds its response back.
    }

{-# DEPRECATED confTimeoutManager "No effect anymore. Use confTimeout" #-}
{-# DEPRECATED confReadNTimeout "No effect anymore. Use confTimeout" #-}

-- | Default config. This is just a template to modify via
--   field names. Don't use this without modifications.
defaultConfig :: Config
defaultConfig =
    Config
        { confWriteBuffer = nullPtr
        , confBufferSize = 0
        , confSendAll = \_ -> return ()
        , confSendAllWith = Nothing
        , confReadN = \_ -> return ""
        , confReadNWith = Nothing
        , confPositionReadMaker = defaultPositionReadMaker
        , confTimeoutManager = T.defaultManager
        , confMySockAddr = SockAddrInet 0 0
        , confPeerSockAddr = SockAddrInet 0 0
        , confReadNTimeout = False
        , confOnInformational = \_ _ -> return ()
        , confTimeout = 0
        }

-- | Sending with 'confSendAllWith' if there is one, giving up when the
--   'STM' action fires, and with 'confSendAll' otherwise.
sendAllGivingUp :: Config -> ByteString -> STM () -> IO (Either () ())
sendAllGivingUp Config{..} bs giveUp = case confSendAllWith of
    Just sendWith -> sendWith bs giveUp
    Nothing -> Right <$> confSendAll bs

-- | Reading with 'confReadNWith' if there is one, giving up when the
--   'STM' action fires, and with 'confReadN' otherwise.
readNGivingUp :: Config -> Int -> STM () -> IO (Either () ByteString)
readNGivingUp Config{..} n giveUp = case confReadNWith of
    Just readWith -> readWith n giveUp
    Nothing -> Right <$> confReadN n

-- | Sending GOAWAY, as a last word: it waits one second at most, since a
--   peer which does not read would hold the connection open for good.
--   Any failure but an asynchronous one is ignored, e.g. TLS refusing to
--   send after a failed send.
sendGoaway :: Config -> ByteString -> IO ()
sendGoaway conf frame = send `E.catch` ignore
  where
    send = case confSendAllWith conf of
        Nothing -> confSendAll conf frame
        Just _ -> do
            late <- registerDelay 1000000
            void $ sendAllGivingUp conf frame (readTVar late >>= check)
    ignore e
        | isAsyncException (e :: E.SomeException) = E.throwIO e
        | otherwise = return ()

-- | Thrown to a thread of the connection's as the last resort, with the
--   error it is told as.  Asynchronous, so that nothing on the way takes
--   it for that error; whoever it was thrown for turns it into the error.
newtype LastResort = LastResort HTTP2Error deriving (Show)

instance E.Exception LastResort where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

-- | Throwing what the last resort carries, once it has arrived.
lastResortAsError :: IO a -> IO a
lastResortAsError = E.handle $ \(LastResort e) -> E.throwIO e

-- | An exception which ended a thread, as another thread is told it: the
--   error of a last resort, and any other asynchronous one wrapped, since
--   the thread told was not the one killed.
forOthers :: E.SomeException -> E.SomeException
forOthers se
    | Just (LastResort e) <- E.fromException se = E.toException e
    | isAsyncException se = E.toException $ BadThingHappen se
    | otherwise = se

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

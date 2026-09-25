{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Types where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IORef
import Foreign.Ptr (nullPtr)
import Network.Control
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.IO
import Network.Socket hiding (Stream)
import System.IO.Unsafe
import qualified System.TimeManager as T

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
>           |             |     HEADERS           DATA           |
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
    | Killed
    | Reset ErrorCode
    | ResetByMe E.SomeException
    deriving (Show)

-- | Used for streams which are cancelled by calling
-- 'Network.HTTP.Semantics.outBodyCancel'.
data CancelledStream = CancelledStream
    deriving (Show, E.Exception)

closedCodeToError :: StreamId -> ClosedCode -> HTTP2Error
closedCodeToError sid cc =
    case cc of
        Finished -> ConnectionIsClosed
        Killed -> ConnectionIsTimeout
        Reset err -> StreamResetIsReceived err sid
        ResetByMe err -> BadThingHappen err

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
    , streamInput :: MVar (Either E.SomeException InpObj) -- Client only
    , streamTxFlow :: TVar TxFlow
    , streamRxFlow :: IORef RxFlow
    , streamRxQ :: IORef (Maybe RxQ)
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
    }

data OutputType
    = OHeader [Header] (Maybe DynaNext) TrailersMaker
    | OPush TokenHeaderList StreamId -- associated stream id from client
    | ONext DynaNext TrailersMaker
    | OInformational [Header]
    | OReset (Maybe E.SomeException)

data Sync = Done | Cont Output

----------------------------------------------------------------

data Control = CFrames (Maybe SettingsList) [ByteString]

----------------------------------------------------------------

type ReasonPhrase = ShortByteString

-- | The connection error or the stream error.
--   A stream error resets that stream and the connection carries on, as
--   RFC 9113 section 5.4.2 asks. One kind of trouble is a connection error
--   even though the spec calls it a stream error, because this
--   implementation cannot carry on through it: a field block abandoned
--   part-way leaves the HPACK tables disagreeing with the peer's, and
--   nothing sent afterwards would decode.
--   `ErrorCode` in connection errors should be the highest stream identifier
--   but in this implementation it identifies the stream that
--   caused this error.
data HTTP2Error
    = ConnectionIsClosed -- NoError
    | ConnectionIsTimeout
    | ConnectionErrorIsReceived ErrorCode StreamId ReasonPhrase
    | ConnectionErrorIsSent ErrorCode StreamId ReasonPhrase
    | StreamErrorIsReceived ErrorCode StreamId
    | StreamResetIsReceived ErrorCode StreamId
    | StreamErrorIsSent ErrorCode StreamId ReasonPhrase
    | BadThingHappen E.SomeException
    deriving (Show)

instance E.Exception HTTP2Error

----------------------------------------------------------------

-- | Checking 'SettingsList' and reporting an error if any.
--
-- >>> checkSettingsList [(SettingsEnablePush,2)]
-- Just (ConnectionErrorIsSent ProtocolError 0 "enable push must be 0 or 1")
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
    , confReadN :: Int -> IO ByteString
    , confPositionReadMaker :: PositionReadMaker
    , confTimeoutManager :: T.Manager
    , confMySockAddr :: SockAddr
    -- ^ This is copied into 'Aux', if exist, on server.
    , confPeerSockAddr :: SockAddr
    -- ^ This is copied into 'Aux', if exist, on server.
    , confReadNTimeout :: Bool
    , confOnInformational :: StreamId -> TokenHeaderTable -> IO ()
    -- ^ Client only: called when a 1xx informational response (e.g. 103 Early
    --   Hints) is received on the given stream, ahead of the final response.
    --   No-op by default.
    --
    --   @since 5.4.2
    }

-- | Default config. This is just a template to modify via
--   field names. Don't use this without modifications.
defaultConfig :: Config
defaultConfig =
    Config
        { confWriteBuffer = nullPtr
        , confBufferSize = 0
        , confSendAll = \_ -> return ()
        , confReadN = \_ -> return ""
        , confPositionReadMaker = defaultPositionReadMaker
        , confTimeoutManager = T.defaultManager
        , confMySockAddr = SockAddrInet 0 0
        , confPeerSockAddr = SockAddrInet 0 0
        , confReadNTimeout = False
        , confOnInformational = \_ _ -> return ()
        }

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

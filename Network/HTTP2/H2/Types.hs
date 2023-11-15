{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Types where

import qualified Control.Exception as E
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.Typeable
import qualified Network.HTTP.Types as H
import System.IO.Unsafe
import UnliftIO.Concurrent
import UnliftIO.Exception (SomeException)
import UnliftIO.STM

import Imports
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.H2.File

----------------------------------------------------------------

-- | "http" or "https".
type Scheme = ByteString

-- | Authority.
type Authority = ByteString

-- | Path.
type Path = ByteString

----------------------------------------------------------------

type InpBody = IO ByteString

data OutBody
    = OutBodyNone
    | -- | Streaming body takes a write action and a flush action.
      OutBodyStreaming ((Builder -> IO ()) -> IO () -> IO ())
    | -- | Like 'OutBodyStreaming', but with a callback to unmask expections
      --
      -- This is used in the client: we spawn the new thread for the request body
      -- with exceptions masked, and provide the body of 'OutBodyStreamingUnmask'
      -- with a callback to unmask them again (typically after installing an exception
      -- handler).
      --
      -- We do /NOT/ support this in the server, as here the scope of the thread
      -- that is spawned for the server is the entire handler, not just the response
      -- streaming body.
      --
      -- TODO: The analogous change for the server-side would be to provide a similar
      -- @unmask@ callback as the first argument in the 'Server' type alias.
      OutBodyStreamingUnmask
        ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    | OutBodyBuilder Builder
    | OutBodyFile FileSpec

-- | Input object
data InpObj = InpObj
    { inpObjHeaders :: HeaderTable
    -- ^ Accessor for headers.
    , inpObjBodySize :: Maybe Int
    -- ^ Accessor for body length specified in content-length:.
    , inpObjBody :: InpBody
    -- ^ Accessor for body.
    , inpObjTrailers :: IORef (Maybe HeaderTable)
    -- ^ Accessor for trailers.
    }

instance Show InpObj where
    show (InpObj (thl, _) _ _body _tref) = show thl

-- | Output object
data OutObj = OutObj
    { outObjHeaders :: [H.Header]
    -- ^ Accessor for header.
    , outObjBody :: OutBody
    -- ^ Accessor for outObj body.
    , outObjTrailers :: TrailersMaker
    -- ^ Accessor for trailers maker.
    }

instance Show OutObj where
    show (OutObj hdr _ _) = show hdr

-- | Trailers maker. A chunks of the response body is passed
--   with 'Just'. The maker should update internal state
--   with the 'ByteString' and return the next trailers maker.
--   When response body reaches its end,
--   'Nothing' is passed and the maker should generate
--   trailers. An example:
--
--   > {-# LANGUAGE BangPatterns #-}
--   > import Data.ByteString (ByteString)
--   > import qualified Data.ByteString.Char8 as C8
--   > import Crypto.Hash (Context, SHA1) -- cryptonite
--   > import qualified Crypto.Hash as CH
--   >
--   > -- Strictness is important for Context.
--   > trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
--   > trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
--   >   where
--   >     !sha1 = C8.pack $ show $ CH.hashFinalize ctx
--   > trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
--   >   where
--   >     !ctx' = CH.hashUpdate ctx bs
--
--   Usage example:
--
--   > let h2rsp = responseFile ...
--   >     maker = trailersMaker (CH.hashInit :: Context SHA1)
--   >     h2rsp' = setResponseTrailersMaker h2rsp maker
type TrailersMaker = Maybe ByteString -> IO NextTrailersMaker

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = return $ Trailers []
defaultTrailersMaker _ = return $ NextTrailersMaker defaultTrailersMaker

-- | Either the next trailers maker or final trailers.
data NextTrailersMaker
    = NextTrailersMaker TrailersMaker
    | Trailers [H.Header]

----------------------------------------------------------------

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)

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

>                        [Open JustOpened]
>                               |
>                               |
>                            HEADERS
>                               |
>                               | (stream1)
>                               |
>                          END_HEADERS?
>                               |
>                        ______/ \______
>                       /   yes   no    \
>                      |                |
>                      |         [Open Continued] <--\
>                      |                |            |
>                      |           CONTINUATION      |
>                      |                |            |
>                      |                | (stream5)  |
>                      |                |            |
>                      |           END_HEADERS?      |
>                      |                |            |
>                      v           yes / \ no        |
>                 END_STREAM? <-------/   \-----------/
>                      |                   (process3)
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
    | Continued
        [HeaderBlockFragment]
        Int -- Total size
        Int -- The number of continuation frames
        Bool -- End of stream
    | NoBody HeaderTable
    | HasBody HeaderTable
    | Body
        (TQueue (Either SomeException ByteString))
        (Maybe Int) -- received Content-Length
        -- compared the body length for error checking
        (IORef Int) -- actual body length
        (IORef (Maybe HeaderTable)) -- trailers

data ClosedCode
    = Finished
    | Killed
    | Reset ErrorCode
    | ResetByMe SomeException
    deriving (Show)

closedCodeToError :: StreamId -> ClosedCode -> HTTP2Error
closedCodeToError sid cc =
    case cc of
        Finished -> ConnectionIsClosed
        Killed -> ConnectionIsTimeout
        Reset err -> ConnectionErrorIsReceived err sid "Connection was reset"
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

data Stream = Stream
    { streamNumber :: StreamId
    , streamState :: IORef StreamState
    , streamWindow :: TVar WindowSize
    , streamInput :: MVar (Either SomeException InpObj) -- Client only
    }

instance Show Stream where
    show Stream{..} =
        "Stream{id="
            ++ show streamNumber
            ++ ",state="
            ++ show (unsafePerformIO (readIORef streamState))
            ++ "}"

----------------------------------------------------------------

data Input a = Input a InpObj

data Output a = Output
    { outputStream :: a
    , outputObject :: OutObj
    , outputType :: OutputType
    , outputStrmQ :: Maybe (TBQueue StreamingChunk)
    , outputSentinel :: IO ()
    }

data OutputType
    = OObj
    | OWait (IO ())
    | OPush TokenHeaderList StreamId -- associated stream id from client
    | ONext DynaNext TrailersMaker

----------------------------------------------------------------

type DynaNext = Buffer -> BufferSize -> WindowSize -> IO Next

type BytesFilled = Int

data Next
    = Next
        BytesFilled -- payload length
        Bool -- require flushing
        (Maybe DynaNext)

----------------------------------------------------------------

data Control
    = CFinish HTTP2Error
    | CFrames (Maybe SettingsList) [ByteString]
    | CGoaway ByteString (MVar ())

----------------------------------------------------------------

data StreamingChunk
    = StreamingFinished (IO ())
    | StreamingFlush
    | StreamingBuilder Builder

----------------------------------------------------------------

type ReasonPhrase = ShortByteString

-- | The connection error or the stream error.
--   Stream errors are treated as connection errors since
--   there are no good recovery ways.
--   `ErrorCode` in connection errors should be the highest stream identifier
--   but in this implementation it identifies the stream that
--   caused this error.
data HTTP2Error
    = ConnectionIsClosed -- NoError
    | ConnectionIsTimeout
    | ConnectionErrorIsReceived ErrorCode StreamId ReasonPhrase
    | ConnectionErrorIsSent ErrorCode StreamId ReasonPhrase
    | StreamErrorIsReceived ErrorCode StreamId
    | StreamErrorIsSent ErrorCode StreamId ReasonPhrase
    | BadThingHappen E.SomeException
    | GoAwayIsSent
    deriving (Show, Typeable)

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

{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Arch.Types where

import qualified Control.Exception as E
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.Typeable
import qualified Network.HTTP.Types as H
import UnliftIO.Concurrent
import UnliftIO.Exception (SomeException)
import UnliftIO.STM

import Imports
import Network.HPACK
import Network.HTTP2.Arch.File
import Network.HTTP2.Frame

----------------------------------------------------------------

-- | "http" or "https".
type Scheme = ByteString

-- | Authority.
type Authority = ByteString

-- | Path.
type Path = ByteString

----------------------------------------------------------------

type InpBody = IO ByteString

data OutBody = OutBodyNone
             -- | Streaming body takes a write action and a flush action.
             | OutBodyStreaming ((Builder -> IO ()) -> IO () -> IO ())
             | OutBodyBuilder Builder
             | OutBodyFile FileSpec

-- | Input object
data InpObj = InpObj {
    inpObjHeaders  :: HeaderTable   -- ^ Accessor for headers.
  , inpObjBodySize :: Maybe Int     -- ^ Accessor for body length specified in content-length:.
  , inpObjBody     :: InpBody       -- ^ Accessor for body.
  , inpObjTrailers :: IORef (Maybe HeaderTable) -- ^ Accessor for trailers.
  }

instance Show InpObj where
    show (InpObj (thl,_) _ _body _tref) = show thl

-- | Output object
data OutObj = OutObj {
    outObjHeaders  :: [H.Header]    -- ^ Accessor for header.
  , outObjBody     :: OutBody       -- ^ Accessor for outObj body.
  , outObjTrailers :: TrailersMaker -- ^ Accessor for trailers maker.
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
--
type TrailersMaker = Maybe ByteString -> IO NextTrailersMaker

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = return $ Trailers []
defaultTrailersMaker _       = return $ NextTrailersMaker defaultTrailersMaker

-- | Either the next trailers maker or final trailers.
data NextTrailersMaker = NextTrailersMaker TrailersMaker
                       | Trailers [H.Header]

----------------------------------------------------------------

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)

----------------------------------------------------------------

data OpenState =
    JustOpened
  | Continued [HeaderBlockFragment]
              Int  -- Total size
              Int  -- The number of continuation frames
              Bool -- End of stream
  | NoBody HeaderTable
  | HasBody HeaderTable
  | Body (TQueue ByteString)
         (Maybe Int) -- received Content-Length
                     -- compared the body length for error checking
         (IORef Int) -- actual body length
         (IORef (Maybe HeaderTable)) -- trailers

data ClosedCode = Finished
                | Killed
                | Reset ErrorCode
                | ResetByMe SomeException
                deriving Show

----------------------------------------------------------------

data StreamState =
    Idle
  | Open OpenState
  | HalfClosedRemote
  | HalfClosedLocal ClosedCode
  | Closed ClosedCode
  | Reserved

instance Show StreamState where
    show Idle                = "Idle"
    show Open{}              = "Open"
    show HalfClosedRemote    = "HalfClosedRemote"
    show (HalfClosedLocal e) = "HalfClosedLocal: " ++ show e
    show (Closed e)          = "Closed: " ++ show e
    show Reserved            = "Reserved"

----------------------------------------------------------------

data Stream = Stream {
    streamNumber     :: StreamId
  , streamState      :: IORef StreamState
  , streamWindow     :: TVar WindowSize
  , streamInput      :: MVar InpObj -- Client only
  }

instance Show Stream where
  show s = show (streamNumber s)

----------------------------------------------------------------

newtype StreamTable = StreamTable (IORef (IntMap Stream))

----------------------------------------------------------------

data Input a = Input a InpObj

data Output a = Output {
    outputStream   :: a
  , outputObject   :: OutObj
  , outputType     :: OutputType
  , outputStrmQ    :: Maybe (TBQueue StreamingChunk)
  , outputSentinel :: IO ()
  }

data OutputType = OObj
                | OWait (IO ())
                | OPush TokenHeaderList StreamId -- associated stream id from client
                | ONext DynaNext TrailersMaker

----------------------------------------------------------------

type DynaNext = Buffer -> BufferSize -> WindowSize -> IO Next

type BytesFilled = Int

data Next = Next BytesFilled (Maybe DynaNext)

----------------------------------------------------------------

data Control = CFinish    HTTP2Error
             | CGoaway    ByteString
             | CFrame     ByteString
             | CSettings  ByteString SettingsList
             | CSettings0 ByteString ByteString SettingsList

----------------------------------------------------------------

data StreamingChunk = StreamingFinished
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
data HTTP2Error =
    ConnectionIsClosed -- NoError
  | ConnectionErrorIsReceived ErrorCode StreamId ReasonPhrase
  | ConnectionErrorIsSent     ErrorCode StreamId ReasonPhrase
  | StreamErrorIsReceived     ErrorCode StreamId
  | StreamErrorIsSent         ErrorCode StreamId
  | BadThingHappen E.SomeException
  deriving (Show, Typeable)

instance E.Exception HTTP2Error

----------------------------------------------------------------

-- | Checking 'SettingsList' and reporting an error if any.
--
-- >>> checkSettingsList [(SettingsEnablePush,2)]
-- Just (ConnectionErrorIsSent ProtocolError 0 "enable push must be 0 or 1")
checkSettingsList :: SettingsList -> Maybe HTTP2Error
checkSettingsList settings = case mapMaybe checkSettingsValue settings of
    []    -> Nothing
    (x:_) -> Just x

checkSettingsValue :: (SettingsKey,SettingsValue) -> Maybe HTTP2Error
checkSettingsValue (SettingsEnablePush,v)
  | v /= 0 && v /= 1 = Just $ ConnectionErrorIsSent ProtocolError 0 "enable push must be 0 or 1"
checkSettingsValue (SettingsInitialWindowSize,v)
  | v > 2147483647   = Just $ ConnectionErrorIsSent FlowControlError 0 "Window size must be less than or equal to 65535"
checkSettingsValue (SettingsMaxFrameSize,v)
  | v < 16384 || v > 16777215 = Just $ ConnectionErrorIsSent ProtocolError 0 "Max frame size must be in between 16384 and 16777215"
checkSettingsValue _ = Nothing

{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Arch.Types where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HTTP2.Arch.File
import Network.HTTP2.Frame
import Network.HTTP2.Priority

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
    show (InpObj (thl,_) _ _body _tref) =
        "Response " ++ show thl ++ " "

-- | Output object
data OutObj = OutObj {
    outObjHeaders  :: [H.Header]    -- ^ Accessor for header.
  , outObjBody     :: OutBody       -- ^ Accessor for outObj body.
  , outObjTrailers :: TrailersMaker -- ^ Accessor for trailers maker.
  }

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
                       | Trailers H.ResponseHeaders

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
              Priority
  | NoBody HeaderTable !Priority
  | HasBody HeaderTable !Priority
  | Body (TQueue ByteString)
         (Maybe Int) -- received Content-Length
                     -- compared the body length for error checking
         (IORef Int) -- actual body length
         (IORef (Maybe HeaderTable)) -- trailers

data ClosedCode = Finished
                | Killed
                | Reset ErrorCodeId
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
  , streamPrecedence :: IORef Precedence
  , streamInput      :: MVar InpObj -- Client only
  }

instance Show Stream where
  show s = show (streamNumber s)

----------------------------------------------------------------

newtype StreamTable = StreamTable (IORef (IntMap Stream))

----------------------------------------------------------------

data Input = Input Stream InpObj

data Output = Output {
    outputStream   :: Stream
  , outputObject   :: OutObj
  , outputType     :: OutputType
  , outputStrmQ    :: Maybe (TBQueue RspStreaming)
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

data Control = CFinish
             | CGoaway    ByteString
             | CFrame     ByteString
             | CSettings  ByteString SettingsList
             | CSettings0 ByteString ByteString SettingsList

----------------------------------------------------------------

data RspStreaming = RSFinish
                  | RSFlush
                  | RSBuilder Builder

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.Arch.Types where

import Control.Concurrent.STM
import Control.Exception (SomeException)
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.IntMap.Strict (IntMap)

import Imports
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.Arch.Object
import Network.HTTP2.Priority

----------------------------------------------------------------

data OpenState =
    JustOpened
  | Continued [HeaderBlockFragment]
              !Int  -- Total size
              !Int  -- The number of continuation frames
              !Bool -- End of stream
              !Priority
  | NoBody HeaderTable !Priority
  | HasBody HeaderTable !Priority
  | Body !(TQueue ByteString)
         !(Maybe Int) -- received Content-Length
                      -- compared the body length for error checking
         !(IORef Int) -- actual body length
         !(IORef (Maybe HeaderTable)) -- trailers

data ClosedCode = Finished
                | Killed
                | Reset !ErrorCodeId
                | ResetByMe SomeException
                deriving Show

----------------------------------------------------------------

data StreamState =
    Idle
  | Open !OpenState
  | HalfClosedRemote
  | HalfClosedLocal !ClosedCode
  | Closed !ClosedCode
  | Reserved

instance Show StreamState where
    show Idle        = "Idle"
    show Open{}      = "Open"
    show HalfClosedRemote  = "HalfClosedRemote"
    show (HalfClosedLocal e)  = "HalfClosedLocal: " ++ show e
    show (Closed e)  = "Closed: " ++ show e
    show Reserved    = "Reserved"

----------------------------------------------------------------

data Stream = Stream {
    streamNumber     :: !StreamId
  , streamState      :: !(IORef StreamState)
  , streamWindow     :: !(TVar WindowSize)
  , streamPrecedence :: !(IORef Precedence)
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
                | OPush !TokenHeaderList !StreamId -- associated stream id from client
                | ONext !DynaNext !TrailersMaker

----------------------------------------------------------------

type DynaNext = Buffer -> BufferSize -> WindowSize -> IO Next

type BytesFilled = Int

data Next = Next !BytesFilled (Maybe DynaNext)

----------------------------------------------------------------

data Control = CFinish
             | CGoaway    !ByteString
             | CFrame     !ByteString
             | CSettings  !ByteString !SettingsList
             | CSettings0 !ByteString !ByteString !SettingsList

----------------------------------------------------------------

data RspStreaming = RSFinish
                  | RSFlush
                  | RSBuilder Builder

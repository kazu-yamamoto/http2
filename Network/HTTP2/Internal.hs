module Network.HTTP2.Internal (
  -- * File
    module Network.HTTP2.Arch.File
  -- * Types
  , Scheme
  , Authority
  , Path
  -- * Request and response
  , InpObj(..)
  , InpBody
  , OutObj(..)
  , OutBody(..)
  , FileSpec(..)
  -- * Trailer
  , TrailersMaker
  , defaultTrailersMaker
  , NextTrailersMaker(..)
  -- * Sender
  , Next(..)
  , BytesFilled
  , DynaNext
  , RspStreaming(..)
  , fillBuilderBodyGetNext
  , fillFileBodyGetNext
  , fillStreamBodyGetNext
  )  where

import Network.HTTP2.Arch.File
import Network.HTTP2.Arch.Types
import Network.HTTP2.Arch.Sender

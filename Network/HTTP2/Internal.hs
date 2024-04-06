module Network.HTTP2.Internal (
    -- * Sender
    Next (..),
    BytesFilled,
    DynaNext,
    StreamingChunk (..),
    fillBuilderBodyGetNext,
    fillFileBodyGetNext,
    fillStreamBodyGetNext,

    -- * Trailer
    TrailersMaker,
    defaultTrailersMaker,
    NextTrailersMaker (..),
    runTrailersMaker,

    -- * Thread Manager
    module Network.HTTP2.H2.Manager,
) where

import Imports
import Network.HTTP2.H2.Manager
import Network.HTTP2.H2.Sender
import Network.HTTP2.H2.Types

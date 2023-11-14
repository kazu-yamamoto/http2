module Network.HTTP2.Internal (
    -- * File
    module Network.HTTP2.H2.File,

    -- * Types
    Scheme,
    Authority,
    Path,

    -- * Request and response
    InpObj (..),
    InpBody,
    OutObj (..),
    OutBody (..),
    FileSpec (..),

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

import Network.HTTP2.H2.File
import Network.HTTP2.H2.Manager
import Network.HTTP2.H2.Sender
import Network.HTTP2.H2.Types

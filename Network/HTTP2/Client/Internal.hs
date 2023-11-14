module Network.HTTP2.Client.Internal (
    Request (..),
    Response (..),

    -- * Low level
    Stream,
    ClientIO (..),
    runIO,
) where

import Network.HTTP2.H2
import Network.HTTP2.Client.Run
import Network.HTTP2.Client.Types

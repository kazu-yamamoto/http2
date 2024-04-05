module Network.HTTP2.Client.Internal (
    Request (..),
    Response (..),
    ClientConfig (..),
    Settings (..),
    Aux (..),

    -- * Low level
    Stream,
    ClientIO (..),
    runIO,
) where

import Network.HTTP.Semantics.Client

import Network.HTTP2.Client.Run
import Network.HTTP2.H2

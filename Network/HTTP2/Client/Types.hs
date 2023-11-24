{-# LANGUAGE RankNTypes #-}

module Network.HTTP2.Client.Types where

import Network.HTTP2.H2

----------------------------------------------------------------

-- | Client type.
type Client a = (forall b. Request -> (Response -> IO b) -> IO b) -> Aux -> IO a

-- | Request from client.
newtype Request = Request OutObj deriving (Show)

-- | Response from server.
newtype Response = Response InpObj deriving (Show)

-- | Additional information.
data Aux = Aux
    { auxPossibleClientStreams :: IO Int
    -- ^ How many streams can be created without blocking.
    , auxServerMaxStreams :: IO Int
    -- ^ Getting server's SETTINGS_MAX_CONCURRENT_STREAMS.
    --   If the server does not inform it,
    --   'maxBound' is used.
    }

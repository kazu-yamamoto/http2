{-# LANGUAGE RankNTypes #-}

module Network.HTTP2.Client.Types where

import Network.HTTP2.H2

----------------------------------------------------------------

-- | Client type.
type Client a = (forall b. Request -> (Response -> IO b) -> IO b) -> IO a

-- | Request from client.
newtype Request = Request OutObj deriving (Show)

-- | Response from server.
newtype Response = Response InpObj deriving (Show)

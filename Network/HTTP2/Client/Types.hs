module Network.HTTP2.Client.Types where

import Network.HTTP2.Arch

import Imports

----------------------------------------------------------------

-- | HTTP\/2 client
type Client a = Request -> (Response -> IO a) -> IO a

-- | Request from client.
type Request = OutObj

-- | Response from server.
type Response = InpObj

-- | "http" or "https"
type Scheme = ByteString

-- | For so-called "Host:" header.
type Authority = ByteString

-- | Path
type Path = ByteString


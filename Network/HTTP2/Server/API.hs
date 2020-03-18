module Network.HTTP2.Server.API where

import qualified System.TimeManager as T

import Network.HTTP2.Arch

----------------------------------------------------------------

-- | HTTP\/2 server takes a HTTP request, should
--   generate a HTTP response and push promises, then
--   should give them to the sending function.
--   The sending function would throw exceptions so that
--   they can be logged.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

type Request = InpObj

type Response = OutObj

-- | Additional information.
data Aux = Aux {
    -- | Time handle for the worker processing this request and response.
    auxTimeHandle :: T.Handle
  }

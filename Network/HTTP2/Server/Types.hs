module Network.HTTP2.Server.Types where

import qualified System.TimeManager as T

import Imports
import Network.HTTP2.Arch
import Network.HTTP2.Frame

----------------------------------------------------------------

-- | HTTP\/2 server takes a HTTP request, should
--   generate a HTTP response and push promises, then
--   should give them to the sending function.
--   The sending function would throw exceptions so that
--   they can be logged.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

-- | Request from client.
type Request = InpObj

-- | Response from server.
type Response = OutObj

-- | HTTP/2 push promise or sever push.
--   Pseudo REQUEST headers in push promise is automatically generated.
--   Then, a server push is sent according to 'promiseResponse'.
data PushPromise = PushPromise {
    -- | Accessor for a URL path in a push promise (a virtual request from a server).
    --   E.g. \"\/style\/default.css\".
      promiseRequestPath :: ByteString
    -- | Accessor for response actually pushed from a server.
    , promiseResponse    :: Response
    -- | Accessor for response weight.
    , promiseWeight      :: Weight
    }

-- | Additional information.
newtype Aux = Aux {
    -- | Time handle for the worker processing this request and response.
    auxTimeHandle :: T.Handle
  }

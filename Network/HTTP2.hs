{-# LANGUAGE OverloadedStrings #-}

-- | Framing in HTTP\/2(<https://tools.ietf.org/html/rfc7540>).
module Network.HTTP2 {-# DEPRECATED "Use Network.HTTP2.Frame instead" #-}
  (
    module Network.HTTP2.Frame
  ) where

import Network.HTTP2.Frame

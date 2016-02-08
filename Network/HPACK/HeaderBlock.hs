module Network.HPACK.HeaderBlock (
  -- * Header block from/to Low level
    HPACKDecoding
  , decodeHeader
  , HPACKEncodingOne
  , prepareEncodeHeader
  ) where

import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Encode

module Network.HPACK.HeaderBlock (
    decodeHeader
  , decodeHeaderTable
  , ValueTable
  , encodeHeader
  , encodeHeaderBuffer
  ) where

import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Encode

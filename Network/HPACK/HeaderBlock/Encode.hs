-- | FIXME

module Network.HPACK.HeaderBlock.Encode (
    toHeaderBlock
  ) where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: HeaderSet
              -> Context
              -> IO (HeaderBlock, Context)
toHeaderBlock = undefined


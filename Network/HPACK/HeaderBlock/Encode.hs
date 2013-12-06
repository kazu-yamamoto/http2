-- | FIXME

module Network.HPACK.HeaderBlock.Encode (
    toHeaderBlock
  ) where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock.Representation

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: HeaderSet
              -> Context
              -> IO (HeaderBlock, Context)
toHeaderBlock = undefined


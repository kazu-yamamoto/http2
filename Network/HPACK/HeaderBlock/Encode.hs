-- | FIXME

module Network.HPACK.HeaderBlock.Encode (
    toHeaderBlock
  ) where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock.Representation

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: HeaderSet
              -> Context
              -> (HeaderBlock, Context)
toHeaderBlock = undefined


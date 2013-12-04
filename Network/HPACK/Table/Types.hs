module Network.HPACK.Table.Types where

import Data.Array (Array)
import Network.HPACK.Table.Entry
import Network.HPACK.Types

-- | Type for table.
type Table = Array Index Entry

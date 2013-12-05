module Network.HPACK.Table.Types where

import Data.Array (Array)
import Network.HPACK.Table.Entry

-- | Type for table.
type Table = Array Index Entry

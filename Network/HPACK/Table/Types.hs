module Network.HPACK.Table.Types where

import Data.Array (Array)
import Network.HPACK.Table.Entry

-- | Index for table.
type Index = Int

-- | Type for table.
type Table = Array Index Entry

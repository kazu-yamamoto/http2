module Network.HPACK.Context (
  -- * Types
    HeaderSet   -- re-exporting
  , Context
  , newContextForEncoding
  , newContextForDecoding
  , changeContextForDecoding
  , DecodeError(..)
  , printContext
  -- * Processing
  , newEntryForEncoding
  , newEntryForDecoding
  -- * Auxiliary functions
  , getEntry
  -- * Table
  , whichTable
  , lookupHeader
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context.HeaderSet
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

-- | Context for HPACK encoding/decoding.
--   This is destructive!
data Context = Context {
    headerTable  :: !HeaderTable -- ^ A cache of headers
  }

-- | Printing 'Context'
printContext :: Context -> IO ()
printContext (Context hdrtbl) = do
    putStrLn "<<<Header table>>>"
    printHeaderTable hdrtbl
    putStr "\n"

----------------------------------------------------------------

-- | Creating a new 'Context'.
--   The first argument is the size of a header table.
newContextForEncoding :: Size -> IO Context
newContextForEncoding maxsiz = Context <$> newHeaderTableForEncoding maxsiz

-- | Creating a new 'Context'.
--   The first argument is the size of a header table.
newContextForDecoding :: Size -> IO Context
newContextForDecoding maxsiz = Context <$> newHeaderTableForDecoding maxsiz

changeContextForDecoding :: Context -> Size -> IO Context
changeContextForDecoding ctx@(Context hdrtbl) siz
  | shouldRenew hdrtbl siz = Context <$> renewHeaderTable siz hdrtbl
  | otherwise              = return ctx

----------------------------------------------------------------

-- | The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntryForEncoding :: Context -> Entry -> IO Context
newEntryForEncoding (Context hdrtbl) e = Context <$> insertEntry e hdrtbl

-- | The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntryForDecoding :: Context -> Entry -> IO Context
newEntryForDecoding (Context hdrtbl) e = Context <$> insertEntry e hdrtbl

----------------------------------------------------------------

-- | Which table does 'Index' refer to?
whichTable :: Index -> Context -> IO (WhichTable, Entry)
whichTable idx ctx = which hdrtbl idx
  where
    hdrtbl = headerTable ctx

-- | Which table contains 'Header'?
lookupHeader :: Header -> Context -> HeaderCache
lookupHeader h ctx = lookupTable h (headerTable ctx)

----------------------------------------------------------------

-- | Getting 'Entry' by 'Index'.
getEntry :: Index -> Context -> IO Entry
getEntry idx ctx = snd <$> whichTable idx ctx

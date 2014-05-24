module Network.HPACK.Context (
  -- * Types
    HeaderSet   -- re-exporting
  , Context
  , newContextForEncoding
  , newContextForDecoding
  , changeContextForDecoding
  , DecodeError(..)
  , printContext
  -- * Initialization and final results
  , emitNotEmittedForEncoding
  , emitNotEmittedForDecoding
  -- * Processing
  , clearRefSets
  , removeRef
  , newEntryForEncoding
  , newEntryForDecoding
  , pushRef
  -- * Auxiliary functions
  , isPresentIn
  , Sequence(..)
  , checkAndUpdate
  , getEntry
  -- * Table
  , whichTable
  , lookupHeader
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context.HeaderSet
import Network.HPACK.Context.ReferenceSet
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

-- | Context for HPACK encoding/decoding.
--   This is destructive!
data Context = Context {
    headerTable  :: !HeaderTable -- ^ A cache of headers
  , referenceSet :: ReferenceSet -- ^ Reference set
  }

-- | Printing 'Context'
printContext :: Context -> IO ()
printContext (Context hdrtbl refs) = do
    putStrLn "<<<Header table>>>"
    printHeaderTable hdrtbl
    putStr "\n"
    putStrLn "<<<Reference set>>>"
    print refs

----------------------------------------------------------------

-- | Creating a new 'Context'.
--   The first argument is the size of a header table.
newContextForEncoding :: Size -> IO Context
newContextForEncoding maxsiz = do
    hdrtbl <- newHeaderTableForEncoding maxsiz
    return $ Context hdrtbl emptyReferenceSet

-- | Creating a new 'Context'.
--   The first argument is the size of a header table.
newContextForDecoding :: Size -> IO Context
newContextForDecoding maxsiz = do
    hdrtbl <- newHeaderTableForDecoding maxsiz
    return $ Context hdrtbl emptyReferenceSet

changeContextForDecoding :: Context -> Size -> IO Context
changeContextForDecoding ctx siz = newContextForDecoding siz -- fixme: copy table

----------------------------------------------------------------

-- | The reference set is emptied.
clearRefSets :: Context -> Context
clearRefSets ctx = ctx {
    referenceSet = emptyReferenceSet
  }

-- | The entry is removed from the reference set.
removeRef :: Context -> Index -> Context
removeRef (Context hdrtbl refs) idx = ctx
  where
    refs' = removeIndex idx refs
    ctx = Context hdrtbl refs'

-- | The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntryForEncoding :: Context -> Entry -> IO ([Index],Context)
newEntryForEncoding (Context hdrtbl refs) e = do
    (hdrtbl', is) <- insertEntry e hdrtbl
    let ns = getCommon is refs
        refs' = addIndex 1 $ adjustReferenceSet $ removeIndices is refs
        ctx = Context hdrtbl' refs'
    return (ns, ctx)

-- | The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntryForDecoding :: Context -> Entry -> IO Context
newEntryForDecoding (Context hdrtbl refs) e = do
    (hdrtbl', is) <- insertEntry e hdrtbl
    let refs' = addIndex 1 $ adjustReferenceSet $ removeIndices is refs
    return $ Context hdrtbl' refs'

-- | The referenced header table entry is added to the reference set.
pushRef :: Context -> Index -> Context
pushRef (Context hdrtbl refs) idx = ctx
  where
    -- isPresentIn ensures that idx does not exist in
    -- newref and oldref.
    refs' = addIndex idx refs
    ctx = Context hdrtbl refs'

----------------------------------------------------------------

-- | Emitting non-emitted headers.
emitNotEmittedForEncoding :: Context -> IO ([Index],Context)
emitNotEmittedForEncoding (Context hdrtbl refs) = do
    let (removedIndces,refs') = renewForEncoding refs
        ctx' = Context hdrtbl refs'
    return (removedIndces, ctx')

-- | Emitting non-emitted headers.
emitNotEmittedForDecoding :: Context -> IO (HeaderSet,Context)
emitNotEmittedForDecoding ctx@(Context hdrtbl refs) = do
    hs <- getNotEmitted ctx
    let refs' = renewForDecoding refs
        ctx'  = Context hdrtbl refs'
    return (hs,ctx')

getNotEmitted :: Context -> IO HeaderSet
getNotEmitted ctx = do
    let is = getNotEmittedIndices $ referenceSet ctx
        hdrtbl = headerTable ctx
    map (fromEntry . snd) <$> mapM (which hdrtbl) is

----------------------------------------------------------------

-- | Is 'Index' present in the reference set?
isPresentIn :: Index -> Context -> Bool
isPresentIn idx ctx = idx `isMember` referenceSet ctx

checkAndUpdate :: Index -> Context -> (Sequence, Context)
checkAndUpdate idx ctx = (s, ctx')
  where
    (s,refs') = lookupAndUpdate idx $ referenceSet ctx
    ctx' = ctx { referenceSet = refs' }

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

{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Context (
  -- * Types
    HeaderSet   -- re-exporting
  , Context
  , newContext
  , DecodeError(..)
  , printContext
  -- * Initialization and final results
  , clearHeaderSet
  , getHeaderSet
  , emitNotEmitted
  -- * Processing
  , clearRefSets
  , removeRef
  , newEntry
  , pushRef
  , emitOnly
  -- * Auxiliary functions
  , isPresentIn
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

-- | Context for decoding.
data Context = Context {
    headerTable     :: !HeaderTable -- ^ A cache of headers
  , oldReferenceSet :: ReferenceSet -- ^ References for not emitted
  , newReferenceSet :: ReferenceSet -- ^ References for already mitted
  , headerSet       :: HeaderSet    -- ^ Emitted header set.
                                    --   Encode: the previous ones.
                                    --   Decode: the results.
  }

-- | Printing 'Context'
printContext :: Context -> IO ()
printContext (Context hdrtbl oldref newref hdrset) = do
    putStrLn "<<<Header table>>>"
    printHeaderTable hdrtbl
    putStr "\n"
    putStrLn "<<<Reference set (old)>>>"
    print $ getIndices oldref
    putStr "\n"
    putStrLn "<<<Reference set (new)>>>"
    print $ getIndices newref
    putStr "\n"
    putStrLn "<<<Headers>>>"
    printHeaderSet hdrset

----------------------------------------------------------------

-- | Creating a new 'Context'.
--   The first argument is the size of 'HeaderTable'.
newContext :: Size -> IO Context
newContext maxsiz = do
    hdrtbl <- newHeaderTable maxsiz
    return $ Context hdrtbl
                     emptyReferenceSet
                     emptyReferenceSet
                     emptyHeaderSet

----------------------------------------------------------------

-- | The reference set is emptied.
clearRefSets :: Context -> IO Context
clearRefSets ctx = return ctx {
    oldReferenceSet = emptyReferenceSet
  , newReferenceSet = emptyReferenceSet
  }

-- | The entry is removed from the reference set.
removeRef :: Context -> Index -> IO Context
removeRef ctx idx = return ctx { oldReferenceSet = removeIndex idx oldref }
  where
    oldref = oldReferenceSet ctx

-- | The header field is emitted.
--   The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntry :: Context -> Entry -> IO Context
newEntry (Context hdrtbl oldref newref hdrset) e = do
    (hdrtbl', is) <- insertEntry e hdrtbl
    let oldref' = removeIndices is $ adjustReferenceSet oldref
        newref' = addIndex 1 $ removeIndices is $ adjustReferenceSet newref
        hdrset' = insertHeader (fromEntry e) hdrset
    return $ Context hdrtbl' oldref' newref' hdrset'

-- | The header field corresponding to the referenced entry is emitted.
--   The referenced header table entry is added to the reference set.
pushRef :: Context -> Index -> Entry -> IO Context
pushRef (Context hdrtbl oldref newref hdrset) idx e = return ctx
  where
    hdrset' = insertHeader (fromEntry e) hdrset
    newref' = addIndex idx newref
    ctx = Context hdrtbl oldref newref' hdrset'

-- | The header field is emitted.
emitOnly :: Context -> Header -> IO Context
emitOnly (Context hdrtbl oldref newref hdrset) h = return ctx
  where
    hdrset' = insertHeader h hdrset
    ctx = Context hdrtbl oldref newref hdrset'

----------------------------------------------------------------

-- | Emitting non-emitted headers.
emitNotEmitted :: Context -> IO Context
emitNotEmitted ctx = emit ctx <$> getNotEmitted ctx

-- | Emit non-emitted headers.
emit :: Context -> HeaderSet -> Context
emit (Context hdrtbl oldref newref hdrset) notEmitted = ctx
  where
    hdrset' = meregeHeaderSet hdrset notEmitted
    oldref' = mergeReferenceSet newref oldref
    ctx = Context hdrtbl oldref' emptyReferenceSet hdrset'

getNotEmitted :: Context -> IO HeaderSet
getNotEmitted ctx = do
    let is = getIndices $ oldReferenceSet ctx
        hdrtbl = headerTable ctx
    map (fromEntry . snd) <$> mapM (which hdrtbl) is

----------------------------------------------------------------

-- | Is 'Index' present in the reference set?
isPresentIn :: Index -> Context -> Bool
isPresentIn idx ctx = idx `isMember` oldref
  where
    oldref = oldReferenceSet ctx

----------------------------------------------------------------

-- | Which table does 'Index' refer to?
whichTable :: Index -> Context -> IO (WhichTable, Entry)
whichTable idx ctx = which hdrtbl idx
  where
    hdrtbl = headerTable ctx

-- | Which table contains 'Header'?
lookupHeader :: Header -> Context -> IO HeaderCache
lookupHeader h ctx = lookupTable h (headerTable ctx)

----------------------------------------------------------------

-- | Getting 'Entry' by 'Index'.
getEntry :: Index -> Context -> IO Entry
getEntry idx ctx = snd <$> whichTable idx ctx

----------------------------------------------------------------

-- | Clearing 'HeaderSet' in 'Context' for the next decode.
clearHeaderSet :: Context -> Context
clearHeaderSet ctx = ctx { headerSet = emptyHeaderSet }

-- | Getting 'HeaderSet' as emitted headers.
getHeaderSet :: Context -> HeaderSet
getHeaderSet = headerSet

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
  , getAndClearHeaderSet
  , emitNotEmittedForEncoding
  , emitNotEmittedForDecoding
  -- * Processing
  , clearRefSets
  , removeRef
  , newEntry
  , pushRef
  , emitOnly
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
data Context = Context {
    headerTable  :: !HeaderTable -- ^ A cache of headers
  , referenceSet :: ReferenceSet -- ^ Reference set
  , headerSet    :: HeaderSet    -- ^ Emitted header set.
                                    --   Encode: the previous ones.
                                    --   Decode: the results.
  }

-- | Printing 'Context'
printContext :: Context -> IO ()
printContext (Context hdrtbl refs hdrset) = do
    putStrLn "<<<Header table>>>"
    printHeaderTable hdrtbl
    putStr "\n"
    putStrLn "<<<Reference set>>>"
    print refs
    putStr "\n"
    putStrLn "<<<Headers>>>"
    printHeaderSet hdrset

----------------------------------------------------------------

-- | Creating a new 'Context'.
--   The first argument is the size of a header table.
newContext :: Size -> IO Context
newContext maxsiz = do
    hdrtbl <- newHeaderTable maxsiz
    return $ Context hdrtbl
                     emptyReferenceSet
                     emptyHeaderSet

----------------------------------------------------------------

-- | The reference set is emptied.
clearRefSets :: Context -> IO Context
clearRefSets ctx = return ctx {
    referenceSet = emptyReferenceSet
  }

-- | The entry is removed from the reference set.
removeRef :: Context -> Index -> IO Context
removeRef (Context hdrtbl refs hdrset) idx = return ctx
  where
    refs' = removeIndex idx refs
    ctx = Context hdrtbl refs' hdrset

-- | The header field is emitted.
--   The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntry :: Context -> Entry -> IO Context
newEntry (Context hdrtbl refs hdrset) e = do
    (hdrtbl', is) <- insertEntry e hdrtbl
    let refs' = addIndex 1 $ removeIndices is $ adjustReferenceSet refs
        hdrset' = insertHeader (fromEntry e) hdrset
    return $ Context hdrtbl' refs' hdrset'

-- | The header field corresponding to the referenced entry is emitted.
--   The referenced header table entry is added to the reference set.
pushRef :: Context -> Index -> Entry -> IO Context
pushRef (Context hdrtbl refs hdrset) idx e = return ctx
  where
    hdrset' = insertHeader (fromEntry e) hdrset
    -- isPresentIn ensures that idx does not exist in
    -- newref and oldref.
    refs' = addIndex idx refs
    ctx = Context hdrtbl refs' hdrset'

-- | The header field is emitted.
emitOnly :: Context -> Header -> IO Context
emitOnly (Context hdrtbl refs hdrset) h = return ctx
  where
    hdrset' = insertHeader h hdrset
    ctx = Context hdrtbl refs hdrset'

----------------------------------------------------------------

-- | Emitting non-emitted headers.
emitNotEmittedForEncoding :: Context -> IO ([Index],Context)
emitNotEmittedForEncoding ctx = emit ctx renewForEncoding <$> getNotEmitted ctx

-- | Emitting non-emitted headers.
emitNotEmittedForDecoding :: Context -> IO Context
emitNotEmittedForDecoding ctx = snd . emit ctx renewForDecoding <$> getNotEmitted ctx

-- renewForEncoding
-- | Emit non-emitted headers.
emit :: Context -> (ReferenceSet -> ([Index],ReferenceSet)) -> HeaderSet
     -> ([Index],Context)
emit (Context hdrtbl refs hdrset) renew notEmitted = (removedIndces,ctx)
  where
    hdrset' = meregeHeaderSet hdrset notEmitted
    (removedIndces,refs') = renew refs
    ctx = Context hdrtbl refs' hdrset'

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

getAndClearHeaderSet :: Context -> (HeaderSet, Context)
getAndClearHeaderSet ctx = (hs, ctx')
  where
    hs = headerSet ctx
    ctx' = clearHeaderSet ctx

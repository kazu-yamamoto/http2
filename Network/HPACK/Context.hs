module Network.HPACK.Context (
  -- * Types
    Context(..)
  , newContext
  -- * Processing
  , emptyRefSets
  , removeRef
  , newEntry
  , pushRef
  , emitOnly
  -- * Auxiliary functions
  , emit
  , doesExist
  ) where

import Network.HPACK.Entry
import Network.HPACK.HeaderTable
import Network.HPACK.ReferenceSet
import Network.HPACK.Types

----------------------------------------------------------------

-- | Context for decoding.
data Context = Context {
    headerTable     :: HeaderTable  -- ^ A cache of headers
  , oldReferenceSet :: ReferenceSet -- ^ References for not emitted
  , newReferenceSet :: ReferenceSet -- ^ References for already mitted
  , headerSet       :: HeaderSet    -- ^ The results
  }

-- FIXME
instance Show Context where
  show (Context hdrtbl oldref _ hdrset) = show hdrtbl ++ "\n"
                                       ++ show oldref ++ "\n"
                                       ++ show hdrset

----------------------------------------------------------------

-- | Creating a new 'Context'.
--   The first argument is the size of 'HeaderTable'.
newContext :: Size -> Context
newContext maxsiz = Context (newHeaderTable maxsiz)
                            emptyReferenceSet
                            emptyReferenceSet
                            emptyHeaderSet

emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

----------------------------------------------------------------

-- | The reference set is emptied.
emptyRefSets :: Context -> Context
emptyRefSets ctx = ctx {
    oldReferenceSet = emptyReferenceSet
  , newReferenceSet = emptyReferenceSet
  }

-- | The entry is removed from the reference set.
removeRef :: Index -> Context -> Context
removeRef idx ctx = ctx { oldReferenceSet = removeIndex idx oldref }
  where
    oldref = oldReferenceSet ctx

-- | The header field is emitted.
--   The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntry :: Entry -> Context -> Context
newEntry e (Context hdrtbl oldref newref hdrset) = ctx
  where
    (hdrtbl', is) = insertEntry e hdrtbl
    oldref' = adjustIndex $ removeIndices is oldref
    newref' = addIndex 1 $ adjustIndex $ removeIndices is newref
    hdrset' = fromEntry e : hdrset
    ctx = Context hdrtbl' oldref' newref' hdrset'

-- | The header field corresponding to the referenced entry is emitted.
--   The referenced header table entry is added to the reference set.
pushRef :: Index -> Entry -> Context -> Context
pushRef idx e (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = fromEntry e : hdrset
    newref' = addIndex idx newref
    ctx = Context hdrtbl oldref newref' hdrset'

-- | The header field is emitted.
emitOnly :: Header -> Context -> Context
emitOnly h (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = h : hdrset
    ctx = Context hdrtbl oldref newref hdrset'

----------------------------------------------------------------

-- | Emit non-emitted headers.
emit :: Context -> HeaderSet -> Context
emit (Context hdrtbl oldref newref hdrset) notEmitted = ctx
  where
    hdrset' = reverse $ notEmitted ++ hdrset
    oldref' = mergeReferenceSet newref oldref
    ctx = Context hdrtbl oldref' emptyReferenceSet hdrset'

----------------------------------------------------------------

-- | Is 'Index' present in the reference set?
doesExist :: Index -> Context -> Bool
doesExist idx ctx = idx `isPresent` oldref
  where
    oldref = oldReferenceSet ctx

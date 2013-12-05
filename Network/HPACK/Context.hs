module Network.HPACK.Context (
  -- * Types
    HeaderSet
  , Context(..)
  , newContext
  -- * Processing
  , emptyRefSets
  , removeRef
  , newEntry
  , pushRef
  , emitOnly
  -- * Auxiliary functions
  , isPresentIn
  , emit
  , notEmittedEntries
  , getEntry
  , switchAction
  ) where

import Data.List (partition)
import Network.HPACK.Context.ReferenceSet
import Network.HPACK.Table

----------------------------------------------------------------

-- | Header set
type HeaderSet = [Header]

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
removeRef :: Context -> Index -> Context
removeRef ctx idx = ctx { oldReferenceSet = removeIndex idx oldref }
  where
    oldref = oldReferenceSet ctx

-- | The header field is emitted.
--   The header field is inserted at the beginning of the header table.
--   A reference to the new entry is added to the reference set.
newEntry :: Context -> Entry -> Context
newEntry (Context hdrtbl oldref newref hdrset) e = ctx
  where
    (hdrtbl', is) = insertEntry e hdrtbl
    oldref' = adjustReferenceSet $ removeIndices is oldref
    newref' = addIndex 1 $ adjustReferenceSet $ removeIndices is newref
    hdrset' = fromEntry e : hdrset
    ctx = Context hdrtbl' oldref' newref' hdrset'

-- | The header field corresponding to the referenced entry is emitted.
--   The referenced header table entry is added to the reference set.
pushRef :: Context -> Index -> Entry -> Context
pushRef (Context hdrtbl oldref newref hdrset) idx e = ctx
  where
    hdrset' = fromEntry e : hdrset
    newref' = addIndex idx newref
    ctx = Context hdrtbl oldref newref' hdrset'

-- | The header field is emitted.
emitOnly :: Context -> Header -> Context
emitOnly (Context hdrtbl oldref newref hdrset) h = ctx
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
isPresentIn :: Index -> Context -> Bool
isPresentIn idx ctx = idx `isMember` oldref
  where
    oldref = oldReferenceSet ctx

----------------------------------------------------------------

-- | Detecting which table does `Index` refer to?
whichTable :: Index -> Context -> WhichTable
whichTable idx ctx = which hdrtbl idx
  where
    hdrtbl = headerTable ctx

----------------------------------------------------------------

-- | Getting 'Entry' by 'Index'.
getEntry :: Index -> Context -> Maybe Entry
getEntry idx ctx = case whichTable idx ctx of
    InHeaderTable e -> Just e
    InStaticTable e -> Just e
    IndexError      -> Nothing

----------------------------------------------------------------

-- | Obtaining non-emitted entries.
notEmittedEntries :: Context -> Maybe [Entry]
notEmittedEntries ctx
  | null ls   = Just xs
  | otherwise = Nothing
  where
    is = getIndices $ oldReferenceSet ctx
    hdrtbl = headerTable ctx
    ws = map (which hdrtbl) is
    (ls,rs) = partition (== IndexError) ws
    xs = map fromWhich rs

----------------------------------------------------------------

-- | Choosing an action depending on which tables.
switchAction :: Context -> Index
             -> (Entry -> Context) -- ^ An action for static table
             -> (Entry -> Context) -- ^ An action for header table
             -> Maybe Context
switchAction ctx idx actionForStatic actionForHeaderTable = case whichTable idx ctx of
    IndexError      -> Nothing
    InStaticTable e -> Just $ actionForStatic e
    InHeaderTable e -> Just $ actionForHeaderTable e

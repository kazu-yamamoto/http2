module Network.HPACK.Context (
    Context(..)
  , newContext
  , newEntry
  , pushRef
  , removeRef
  , emptyRefSets
  , emitOnly
  , emit
  , whichTable
  , allEnts
  , doesExist
  ) where

import Data.List (partition)
import Network.HPACK.Entry
import Network.HPACK.HeaderTable
import Network.HPACK.ReferenceSet
import Network.HPACK.Types

----------------------------------------------------------------

data Context = Context {
    headerTable :: HeaderTable
  , oldReferenceSet :: ReferenceSet -- not emitted
  , newReferenceSet :: ReferenceSet -- emitted
  , headerSet :: HeaderSet
  }

-- FIXME
instance Show Context where
  show (Context hdrtbl oldref _ hdrset) = show hdrtbl ++ "\n"
                                       ++ show oldref ++ "\n"
                                       ++ show hdrset

----------------------------------------------------------------

newContext :: Size -> Context
newContext maxsiz = Context (newHeaderTable maxsiz)
                            emptyReferenceSet
                            emptyReferenceSet
                            emptyHeaderSet

emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

----------------------------------------------------------------

newEntry :: Entry -> Context -> Context
newEntry e (Context hdrtbl oldref newref hdrset) = ctx
  where
    (hdrtbl', is) = insertEntry e hdrtbl
    oldref' = adjustIndex $ removeIndices is oldref
    newref' = addIndex 1 $ adjustIndex $ removeIndices is newref
    hdrset' = fromEntry e : hdrset
    ctx = Context hdrtbl' oldref' newref' hdrset'

pushRef :: Index -> Entry -> Context -> Context
pushRef idx e (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = fromEntry e : hdrset
    newref' = addIndex idx newref
    ctx = Context hdrtbl oldref newref' hdrset'

removeRef :: Index -> Context -> Context
removeRef idx ctx = ctx { oldReferenceSet = removeIndex idx oldref }
  where
    oldref = oldReferenceSet ctx

emptyRefSets :: Context -> Context
emptyRefSets ctx = ctx {
    oldReferenceSet = emptyReferenceSet
  , newReferenceSet = emptyReferenceSet
  }

emitOnly :: Header -> Context -> Context
emitOnly h (Context hdrtbl oldref newref hdrset) = ctx
  where
    hdrset' = h : hdrset
    ctx = Context hdrtbl oldref newref hdrset'

----------------------------------------------------------------

emit :: Context -> HeaderSet -> Context
emit (Context hdrtbl oldref newref hdrset) notEmitted = ctx
  where
    hdrset' = reverse $ notEmitted ++ hdrset
    oldref' = mergeReferenceSet newref oldref
    ctx = Context hdrtbl oldref' emptyReferenceSet hdrset'

----------------------------------------------------------------

whichTable :: Index -> Context -> WhichTable
whichTable idx ctx = hdrtbl .!. idx
  where
    hdrtbl = headerTable ctx

allEnts :: Context -> ([WhichTable], [WhichTable])
allEnts ctx = partition (== IndexError) ws
  where
    ReferenceSet is = oldReferenceSet ctx
    hdrtbl = headerTable ctx
    ws = map (\i -> hdrtbl .!. i) is

doesExist :: Index -> Context -> Bool
doesExist idx ctx = idx `isPresent` oldref
  where
    oldref = oldReferenceSet ctx

module Network.HPACK.Context.HeaderSet where

import Network.HPACK.Table

-- | Header set.
type HeaderSet = [Header]

-- | Empty header set.
emptyHeaderSet :: HeaderSet
emptyHeaderSet = []

-- | Merging the emitted header set and the non-emitted header set.
meregeHeaderSet :: HeaderSet -> HeaderSet -> HeaderSet
meregeHeaderSet hdrset notEmitted = reverse $ notEmitted ++ hdrset

-- | Inserting 'Header' to 'HeaderSet'.
insertHeader :: Header -> HeaderSet -> HeaderSet
insertHeader = (:)


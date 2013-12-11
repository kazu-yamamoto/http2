{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.Decode (
    fromHeaderBlock
  , decodeStep
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

----------------------------------------------------------------

-- FIXME: this is not necessary
-- | Errors for decoder.
data DecodeError = IndexOverrun -- ^ Index is out of the range
                 deriving Show

-- | Decoding 'HeaderBlock' to 'HeaderSet'.
fromHeaderBlock :: HeaderBlock
                -> Context
                -> IO (HeaderSet, Context)
fromHeaderBlock (r:rs) !ctx = decodeStep ctx r >>= fromHeaderBlock rs
fromHeaderBlock [] !ctx = do
    notEmitted <- getNotEmitted ctx
    let !ctx' = emit ctx notEmitted
        !hs = getHeaderSet ctx'
        !ctx'' = clearHeaderSet ctx'
    return (hs, ctx'')

----------------------------------------------------------------

-- | Decoding step for one 'Representation'. Exporting for the
--   test purpose.
decodeStep :: Context -> Representation -> IO Context
decodeStep !ctx (Indexed idx)
  | idx == 0  = clearRefSets ctx
  | isPresent = removeRef ctx idx
  | otherwise = switchAction ctx idx forStatic forHeaderTable
  where
    isPresent = idx `isPresentIn` ctx
    forStatic = newEntry ctx
    forHeaderTable = pushRef ctx idx
decodeStep !ctx (Literal NotAdd naming v) = do
    k <- fromNaming naming ctx
    emitOnly ctx (k,v)
decodeStep !ctx (Literal Add naming v) = do
    k <- fromNaming naming ctx
    newEntry ctx $ toEntry (k,v)

----------------------------------------------------------------

fromNaming :: Naming -> Context -> IO HeaderName
fromNaming (Lit k)   _   = return k
fromNaming (Idx idx) ctx = entryHeaderName <$> getEntry idx ctx

----------------------------------------------------------------

getNotEmitted :: Context -> IO HeaderSet
getNotEmitted ctx = map fromEntry <$> notEmittedEntries ctx

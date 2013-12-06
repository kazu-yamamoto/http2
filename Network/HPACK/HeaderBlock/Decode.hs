module Network.HPACK.HeaderBlock.Decode (
    fromHeaderBlock
  , decodeStep
  ) where

import Network.HPACK.Context
import Network.HPACK.HeaderBlock.Representation
import Network.HPACK.Table

----------------------------------------------------------------

-- FIXME: this is not necessary
-- | Errors for decoder.
data DecodeError = IndexOverrun -- ^ Index is out of the range
                 deriving Show

-- | Decoding 'HeaderBlock' to 'HeaderSet'.
fromHeaderBlock :: HeaderBlock
                -> Context
                -> Maybe (HeaderSet, Context)
fromHeaderBlock (r:rs) ctx = case decodeStep ctx r of
    Left  _    -> Nothing
    Right ctx' -> fromHeaderBlock rs ctx'
fromHeaderBlock [] ctx = case getNotEmitted ctx of
    Left  _          -> Nothing
    Right notEmitted -> let ctx' = emit ctx notEmitted
                            hs = getHeaderSet ctx'
                            ctx'' = clearHeaderSet ctx'
                        in Just (hs, ctx'')

----------------------------------------------------------------

-- | Decoding step for one 'Representation'. Exporting for the
--   test purpose.
decodeStep :: Context -> Representation -> Either DecodeError Context
decodeStep ctx (Indexed idx)
  | idx == 0  = Right $ clearRefSets ctx
  | isPresent = Right $ removeRef ctx idx
  | otherwise = case switchAction ctx idx forStatic forHeaderTable of
      Nothing   -> Left IndexOverrun
      Just ctx' -> Right ctx'
  where
    isPresent = idx `isPresentIn` ctx
    forStatic = newEntry ctx
    forHeaderTable = pushRef ctx idx
decodeStep ctx (Literal NotAdd naming v) = case fromNaming naming ctx of
    Right k -> Right $ emitOnly ctx (k,v)
    Left  e -> Left e
decodeStep ctx (Literal Add naming v) = case fromNaming naming ctx of
    Right k -> Right $ newEntry ctx $ toEntry (k,v)
    Left  e -> Left e

----------------------------------------------------------------

fromNaming :: Naming -> Context -> Either DecodeError HeaderName
fromNaming (Lit k)   _   = Right k
fromNaming (Idx idx) ctx = case getEntry idx ctx of
    Nothing -> Left IndexOverrun
    Just e  -> Right $ entryHeaderName e

----------------------------------------------------------------

getNotEmitted :: Context -> Either DecodeError HeaderSet
getNotEmitted ctx = case notEmittedEntries ctx of
    Nothing -> Left IndexOverrun
    Just xs -> Right $ map fromEntry xs

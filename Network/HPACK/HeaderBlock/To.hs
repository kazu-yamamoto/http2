{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: HeaderSet
              -> Context
              -> IO (HeaderBlock, Context)
toHeaderBlock hs !ctx = encodeInit ctx >>= toHeaderBlock' hs

toHeaderBlock' :: HeaderSet
               -> Context
               -> IO (HeaderBlock, Context)
toHeaderBlock' (h:hs) !ctx = encodeStep ctx h >>= toHeaderBlock' hs
toHeaderBlock' []     !ctx = encodeFinal ctx

encodeStep :: Context -> Header -> IO Context
encodeStep !ctx h@(k,v) = do
    cache <- lookupTable2 ctx h
    let e = toEntry h
    case cache of
        None -> do
            let hf = Literal Add (Lit k) v
            pushHeaderField hf <$> newEntry ctx e
        KeyOnly InStaticTable i  -> do
            let hf = Literal Add (Idx i) v
            pushHeaderField hf <$> newEntry ctx e
        KeyOnly InHeaderTable i  -> do
            let hf = Literal Add (Idx i) v
            pushHeaderField hf <$> newEntry ctx e
        KeyValue InStaticTable i -> do
            let hf = Indexed i
            pushHeaderField hf <$> newEntry ctx e
        KeyValue InHeaderTable i -> do
            let hf = Indexed i
            pushHeaderField hf <$> pushRef ctx i e


encodeInit :: Context -> IO Context
encodeInit ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx -- FIXME: in the case of diff
    return $ setHeaderBlock ctx' [Indexed 0] -- FIXME

encodeFinal :: Context -> IO (HeaderBlock, Context)
encodeFinal ctx = do
    !ctx' <- emitNotEmitted ctx
    let !hb = reverse $ getHeaderBlock ctx' -- FIXME
    return (hb, ctx')

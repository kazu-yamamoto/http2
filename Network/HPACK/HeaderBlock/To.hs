{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.To (
    toHeaderBlock
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Builder
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table
import Network.HPACK.Types

type Ctx = (Context, Builder HeaderField)
type Step = Ctx -> Header -> IO Ctx

-- | Encoding 'HeaderSet' to 'HeaderBlock'.
toHeaderBlock :: CompressionAlgo
              -> Context
              -> HeaderSet
              -> IO (Context, HeaderBlock)
toHeaderBlock Naive  !ctx hs = encodeInit ctx >>= encodeStep naiveStep  hs
toHeaderBlock Linear !ctx hs = encodeInit ctx >>= encodeStep linearStep hs
toHeaderBlock _      _    _  = undefined -- fixme

----------------------------------------------------------------

encodeInit :: Context -> IO Ctx
encodeInit ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx
    let initialHeaderBlock = singleton $ Indexed 0
    return (ctx', initialHeaderBlock)

encodeFinal :: Ctx -> IO (Context, HeaderBlock)
encodeFinal (ctx, builder) = do
    !ctx' <- emitNotEmitted ctx
    let !hb = run builder
    return (ctx', hb)

encodeStep :: Step
           -> HeaderSet
           -> Ctx
           -> IO (Context, HeaderBlock)
encodeStep step (h:hs) !ctx = step ctx h >>= encodeStep step hs
encodeStep _    []     !ctx = encodeFinal ctx

----------------------------------------------------------------

naiveStep :: Step
naiveStep (!ctx,!builder) (k,v) = do
    let builder' = builder << Literal NotAdd (Lit k) v
    ctx' <- emitOnly ctx (k,v)
    return (ctx', builder')

----------------------------------------------------------------
-- A simple encoding strategy to reset the reference set first
-- by 'Index 0' and uses indexing as much as possible.

linearStep :: Step
linearStep (!ctx,!builder) h@(k,v) = do
    cache <- lookupHeader h ctx
    let e = toEntry h
    case cache of
        None -> do
            let builder' = builder << Literal Add (Lit k) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyOnly InStaticTable i  -> do
            let builder' = builder << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyOnly InHeaderTable i  -> do
            let builder' = builder << Literal Add (Idx i) v
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyValue InStaticTable i -> do
            let builder' = builder << Indexed i
            ctx' <- newEntry ctx e
            return (ctx', builder')
        KeyValue InHeaderTable i -> do
            (builder',ctx') <- if i `isPresentIn` ctx then do
                  let b = builder << Indexed i << Indexed i
                      c = ctx
                  return (b,c)
                else do
                  let b = builder << Indexed i
                  c <- pushRef ctx i e
                  return (b,c)
            return (ctx', builder')

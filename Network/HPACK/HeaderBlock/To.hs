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
toHeaderBlock Naive  !ctx hs = reset ctx >>= encodeStep naiveStep  hs
toHeaderBlock Linear !ctx hs = reset ctx >>= encodeStep linearStep hs
toHeaderBlock _      _    _  = undefined -- fixme

----------------------------------------------------------------

encodeFinal :: Ctx -> IO (Context, HeaderBlock)
encodeFinal (ctx, builder) = do
    !ctx' <- emitNotEmittedForEncoding ctx
    let !hb = run builder
    return (ctx', hb)

encodeStep :: Step
           -> HeaderSet
           -> Ctx
           -> IO (Context, HeaderBlock)
encodeStep step (h:hs) !ctx = step ctx h >>= encodeStep step hs
encodeStep _    []     !ctx = encodeFinal ctx

----------------------------------------------------------------

-- for naiveStep and linearStep
reset :: Context -> IO Ctx
reset ctx = do
    ctx' <- clearHeaderSet <$> clearRefSets ctx
    let initialHeaderBlock = singleton $ Indexed 0
    return (ctx', initialHeaderBlock)

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
linearStep cb@(!ctx,!builder) h = anyStep linear cb h
  where
    linear i e
      | i `isPresentIn` ctx = do
          let b = builder << Indexed i << Indexed i
              c = ctx
          return (c,b)
      | otherwise = do
          let b = builder << Indexed i
          c <- pushRef ctx i e
          return (c,b)

----------------------------------------------------------------
-- See http://lists.w3.org/Archives/Public/ietf-http-wg/2013JulSep/1135.html

diffStep :: Step
diffStep cb@(!ctx,!builder) h@(k,v) = anyStep diff cb h
  where
    diff = undefined

----------------------------------------------------------------

anyStep :: (Index -> Entry -> IO Ctx) -> Step
anyStep func (!ctx,!builder) h@(k,v) = do
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
        KeyValue InHeaderTable i -> func i e

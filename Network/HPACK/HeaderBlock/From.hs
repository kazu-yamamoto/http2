{-# LANGUAGE BangPatterns #-}

module Network.HPACK.HeaderBlock.From (
    fromHeaderBlock
  , decodeStep
  ) where

import Control.Applicative ((<$>))
import Network.HPACK.Builder
import Network.HPACK.Context
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table

----------------------------------------------------------------

type Ctx = (Context, Builder Header)
type Step = Ctx -> HeaderField -> IO Ctx

-- | Decoding 'HeaderBlock' to 'HeaderList'.
fromHeaderBlock :: Context
                -> HeaderBlock
                -> IO (Context, HeaderList)
fromHeaderBlock !ctx rs = decodeLoop rs (ctx,empty)

----------------------------------------------------------------

decodeLoop :: HeaderBlock -> Ctx -> IO (Context, HeaderList)
decodeLoop (r:rs) !ctx = decodeStep ctx r >>= decodeLoop rs
decodeLoop []     !ctx = decodeFinal ctx

----------------------------------------------------------------

-- | Decoding step for one 'HeaderField'. Exporting for the
--   test purpose.
decodeStep :: Step
decodeStep (!ctx,!builder) (ChangeTableSize siz) = do
    ctx' <- changeContextForDecoding ctx siz
    return (ctx',builder)
decodeStep (!ctx,!builder) (Indexed idx) = do
      w <- whichTable idx ctx
      case w of
          (InStaticTable, e) -> do
              let b = builder << fromEntry e
              return (ctx,b)
          (InHeaderTable, e) -> do
              let c = ctx
                  b = builder << fromEntry e
              return (c,b)
decodeStep (!ctx,!builder) (Literal NotAdd naming v) = do
    k <- fromNaming naming ctx
    let b = builder << (k,v)
    return (ctx, b)
decodeStep (!ctx,!builder) (Literal Never naming v) = do
    k <- fromNaming naming ctx
    let b = builder << (k,v)
    return (ctx, b)
decodeStep (!ctx,!builder) (Literal Add naming v) = do
    k <- fromNaming naming ctx
    let h = (k,v)
        e = toEntry (k,v)
        b = builder << h
    c <- newEntryForDecoding ctx e
    return (c,b)

decodeFinal :: Ctx -> IO (Context, HeaderList)
decodeFinal (!ctx, !builder) = return (ctx, run builder)

----------------------------------------------------------------

fromNaming :: Naming -> Context -> IO HeaderName
fromNaming (Lit k)   _   = return k
fromNaming (Idx idx) ctx = entryHeaderName <$> getEntry idx ctx

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

-- | Decoding 'HeaderBlock' to 'HeaderSet'.
fromHeaderBlock :: Context
                -> HeaderBlock
                -> IO (Context, HeaderSet)
fromHeaderBlock !ctx rs = decodeLoop rs (ctx,empty)

----------------------------------------------------------------

decodeLoop :: HeaderBlock -> Ctx -> IO (Context, HeaderSet)
decodeLoop (r:rs) !ctx = decodeStep ctx r >>= decodeLoop rs
decodeLoop []     !ctx = decodeFinal ctx

----------------------------------------------------------------

-- | Decoding step for one 'HeaderField'. Exporting for the
--   test purpose.
decodeStep :: Step
decodeStep (!ctx,!builder) Clear = return (clearRefSets ctx,builder)
decodeStep (!ctx,!builder) (ChangeTableSize siz) = error "ChangeTableSize"
decodeStep (!ctx,!builder) (Indexed idx)
  | isPresent = return (removeRef ctx idx, builder)
  | otherwise = do
      w <- whichTable idx ctx
      case w of
          (InStaticTable, e) -> do
              c <- newEntryForDecoding ctx e
              let b = builder << fromEntry e
              return (c,b)
          (InHeaderTable, e) -> do
              let c = pushRef ctx idx
                  b = builder << fromEntry e
              return (c,b)
  where
    isPresent = idx `isPresentIn` ctx
decodeStep (!ctx,!builder) (Literal NotAdd naming v) = do
    k <- fromNaming naming ctx
    let b = builder << (k,v)
    return (ctx, b)
-- fixme: how to treat Never?
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

decodeFinal :: Ctx -> IO (Context, HeaderSet)
decodeFinal (!ctx, !builder) = do
    (hs,!ctx') <- emitNotEmittedForDecoding ctx
    let hs' = run builder ++ hs
    return (ctx', hs')

----------------------------------------------------------------

fromNaming :: Naming -> Context -> IO HeaderName
fromNaming (Lit k)   _   = return k
fromNaming (Idx idx) ctx = entryHeaderName <$> getEntry idx ctx

{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK.HeaderBlock.From (
    fromHeaderBlock
  , decodeStep
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Control.Monad (unless)
import Network.HPACK.Builder
import Network.HPACK.HeaderBlock.HeaderField
import Network.HPACK.Table
import Network.HPACK.Types

----------------------------------------------------------------

type Ctx = (DynamicTable, Builder Header)
type Step = Ctx -> HeaderField -> IO Ctx

-- | Decoding 'HeaderBlock' to 'HeaderList'.
fromHeaderBlock :: DynamicTable
                -> HeaderBlock
                -> IO (DynamicTable, HeaderList)
fromHeaderBlock !dyntbl rs = decodeLoop rs (dyntbl,empty)

----------------------------------------------------------------

decodeLoop :: HeaderBlock -> Ctx -> IO (DynamicTable, HeaderList)
decodeLoop (r:rs) !dyntbl = decodeStep dyntbl r >>= decodeLoop rs
decodeLoop []     !dyntbl = decodeFinal dyntbl

----------------------------------------------------------------

-- | Decoding step for one 'HeaderField'. Exporting for the
--   test purpose.
decodeStep :: Step
decodeStep (!dyntbl,!builder) (ChangeTableSize siz) = do
    unless (isSuitableSize siz dyntbl) $ throwIO TooLargeTableSize
    dyntbl' <- renewDynamicTable siz dyntbl
    return (dyntbl',builder)
decodeStep (!dyntbl,!builder) (Indexed idx) = do
      w <- which dyntbl idx
      case w of
          (InStaticTable, e) -> do
              let b = builder << fromEntry e
              return (dyntbl,b)
          (InDynamicTable, e) -> do
              let b = builder << fromEntry e
              return (dyntbl,b)
decodeStep (!dyntbl,!builder) (Literal NotAdd naming v) = do
    k <- fromNaming naming dyntbl
    let b = builder << (k,v)
    return (dyntbl, b)
decodeStep (!dyntbl,!builder) (Literal Never naming v) = do
    k <- fromNaming naming dyntbl
    let b = builder << (k,v)
    return (dyntbl, b)
decodeStep (!dyntbl,!builder) (Literal Add naming v) = do
    k <- fromNaming naming dyntbl
    let h = (k,v)
        e = toEntry (k,v)
        b = builder << h
    dyntbl' <- insertEntry e dyntbl
    return (dyntbl',b)

decodeFinal :: Ctx -> IO (DynamicTable, HeaderList)
decodeFinal (!dyntbl, !builder) = return (dyntbl, run builder)

----------------------------------------------------------------

fromNaming :: Naming -> DynamicTable -> IO HeaderName
fromNaming (Lit k)   _   = return k
fromNaming (Idx idx) dyntbl = entryHeaderName . snd <$> which dyntbl idx

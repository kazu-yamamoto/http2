{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK2.HeaderBlock.From (
    fromHeaderBlock
  , decodeStep
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (throwIO)
import Control.Monad (unless)
import Network.HPACK2.Builder
import Network.HPACK2.HeaderBlock.HeaderField
import Network.HPACK2.Table
import Network.HPACK2.Types

----------------------------------------------------------------

type Ctx = (DynamicTable, Builder Header, Bool)
type Step = Ctx -> HeaderField -> IO Ctx

-- | Decoding 'HeaderBlock' to 'HeaderList'.
fromHeaderBlock :: DynamicTable
                -> HeaderBlock
                -> IO (DynamicTable, HeaderList)
fromHeaderBlock !dyntbl rs = decodeLoop rs (dyntbl,empty,True)

----------------------------------------------------------------

decodeLoop :: HeaderBlock -> Ctx -> IO (DynamicTable, HeaderList)
decodeLoop (r:rs) !dyntbl = decodeStep dyntbl r >>= decodeLoop rs
decodeLoop []     !dyntbl = decodeFinal dyntbl

----------------------------------------------------------------

-- | Decoding step for one 'HeaderField'. Exporting for the
--   test purpose.
decodeStep :: Step
decodeStep (!dyntbl,!builder,beginning) (ChangeTableSize siz)
  | beginning = do
    unless (isSuitableSize siz dyntbl) $ throwIO TooLargeTableSize
    dyntbl' <- renewDynamicTable siz dyntbl
    return (dyntbl',builder,True)
  | otherwise = throwIO IllegalTableSizeUpdate
decodeStep (!dyntbl,!builder,_) (Indexed idx) = do
      w <- which dyntbl idx
      case w of
          (InStaticTable, e) -> do
              let b = builder << fromEntry e
              return (dyntbl,b,False)
          (InDynamicTable, e) -> do
              let b = builder << fromEntry e
              return (dyntbl,b,False)
decodeStep (!dyntbl,!builder,_) (Literal NotAdd naming v) = do
    k <- fromNaming naming dyntbl
    let b = builder << (k,v)
    return (dyntbl, b, False)
decodeStep (!dyntbl,!builder,_) (Literal Never naming v) = do
    k <- fromNaming naming dyntbl
    let b = builder << (k,v)
    return (dyntbl, b, False)
decodeStep (!dyntbl,!builder,_) (Literal Add naming v) = do
    k <- fromNaming naming dyntbl
    let h = (k,v)
        e = toEntry (k,v)
        b = builder << h
    dyntbl' <- insertEntry e dyntbl
    return (dyntbl',b,False)

decodeFinal :: Ctx -> IO (DynamicTable, HeaderList)
decodeFinal (!dyntbl,!builder,_) = return (dyntbl, run builder)

----------------------------------------------------------------

fromNaming :: Naming -> DynamicTable -> IO HeaderName
fromNaming (Lit k)   _   = return k
fromNaming (Idx idx) dyntbl = entryHeaderName . snd <$> which dyntbl idx

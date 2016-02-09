{-# LANGUAGE CPP #-}

module HPACK.EncodeSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (onException)
import Network.HPACK
import Test.Hspec

spec :: Spec
spec = do
    describe "encodeHeader and decodeHeader" $ do
        it "works for Naive" $
            run EncodeStrategy {compressionAlgo = Naive, useHuffman = False}
        it "works for NaiveH" $
            run EncodeStrategy {compressionAlgo = Naive, useHuffman = True}
        it "works for Static" $
            run EncodeStrategy {compressionAlgo = Static, useHuffman = False}
        it "works for StaticH" $
            run EncodeStrategy {compressionAlgo = Static, useHuffman = True}
        it "works for Linear" $
            run EncodeStrategy {compressionAlgo = Linear, useHuffman = False}
        it "works for LinearH" $
            run EncodeStrategy {compressionAlgo = Linear, useHuffman = True}

run :: EncodeStrategy -> Expectation
run stgy = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    withDynamicTableForEncoding defaultDynamicTableSize $ \etbl ->
        withDynamicTableForDecoding defaultDynamicTableSize 4096 $ \dtbl ->
        go etbl dtbl stgy hdrs `shouldReturn` True

go :: DynamicTable -> DynamicTable -> EncodeStrategy -> [HeaderList] -> IO Bool
go _    _    _    []     = return True
go etbl dtbl stgy (h:hs) = do
    bs <- encodeHeader stgy 4096 etbl h `onException` print h
    h' <- decodeHeader dtbl bs `onException` print h
    if h == h' then
        go etbl dtbl stgy hs
      else do
        return False

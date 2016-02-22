{-# LANGUAGE CPP #-}

module HPACK.EncodeSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (onException)
import qualified Data.ByteString as BS
import Network.HPACK
import Test.Hspec

spec :: Spec
spec = do
    describe "encodeHeader and decodeHeader" $ do
        it "works for Naive" $
            run EncodeStrategy {compressionAlgo = Naive, useHuffman = False} []
        it "works for NaiveH" $
            run EncodeStrategy {compressionAlgo = Naive, useHuffman = True} []
        it "works for Static" $
            run EncodeStrategy {compressionAlgo = Static, useHuffman = False} []
        it "works for StaticH" $
            run EncodeStrategy {compressionAlgo = Static, useHuffman = True} []
        it "works for Linear" $
            run EncodeStrategy {compressionAlgo = Linear, useHuffman = False} linearLens
        it "works for LinearH" $
            run EncodeStrategy {compressionAlgo = Linear, useHuffman = True} []

run :: EncodeStrategy -> [Int] -> Expectation
run stgy lens = do
    hdrs <- read <$> readFile "bench-hpack/headers.hs"
    withDynamicTableForEncoding defaultDynamicTableSize $ \etbl ->
        withDynamicTableForDecoding defaultDynamicTableSize 4096 $ \dtbl ->
        go etbl dtbl stgy hdrs lens `shouldReturn` True

go :: DynamicTable -> DynamicTable -> EncodeStrategy -> [HeaderList] -> [Int] -> IO Bool
go _    _    _    []     _    = return True
go etbl dtbl stgy (h:hs) lens = do
    bs <- encodeHeader stgy 4096 etbl h `onException` print h
    let lens' = case lens of
            l:ls
              | BS.length bs == l -> ls
              | otherwise         -> error $ show h ++ "\nshould be " ++ show l
            []                    -> []
    h' <- decodeHeader dtbl bs `onException` print h
    if h == h' then
        go etbl dtbl stgy hs lens'
      else do
        return False

linearLens :: [Int]
linearLens = [250,312,26,390,288,204,224,204,200,202,204,204,206,206,228,100,204,204,218,208,228,434,208,608,232,208,208,208,98,202,208,256,168,208,208,224,208,208,382,84,242,208,208,232,208,208,208,210,210,210,210,208,210,222,208,210,400,224,238,206,206,230,252,222,202,202,198,138,250,204,216,204,204,108,96,306,250,242,208,94,226,206,264,222,40,224,810,204,38,266,144,158,254,100,206,110,132,38,254,144,102,132,102,102,102,102,102,210,230,208,204,464,224,142,198,198,410,156,250,218,130,18,26,338,284,238,222,36,142,208,92,34,552,152,206,1020,288,42,490,98,40,1884,434,300,240,206,278,278,268,252,460,632,178,220,298,144,430,746,724,202,330,144,204,206,782,146,206,206,146,240,228,204,206,208,300,144,160,146,146,38,280,220,144,146,100,144,418,206,204,294,144,300,228,204,204,146,144,240,204,244,218,230,286,102,256,202,208,206,144,146,206,836,204,842,300,220,326,182,300,148,150,204,144,144,98,146,204,206,146,100,204,222,202,202,166,268,146,40,38,142,38,206,418,318,226,174,256,246,274,208,208,208,208,208,544,254,146,146,144,268,160,572,362,178,224,590,362,3150,1034,316,402,204,228,206,206,40,146,142,266,158,142,354,380,264,702,74,424,674,410,688,322,250,300,204,188,60,298,204,206,468,230,200,232,222,208,210,272,282,252,218,724,144,238,206,208,210,100,254,146,144,124,38,112,204,204,216,168,208,276,100,206,116,100,326,892,194,102,210,102,210,206,40,126,102,100,208,98,242,206,218,278,282,292,234,144,40,144,202,288,206,98,40,146,148,40,116,850,242,38,40,40,148,204,110,290,162,662,212,218,230,100,100,134,100,1026,100,2442,100,100,100,208,100,100,112,100,164,144,100,100,100,110,100,518,202,232,342,728,46,384,204,230,100,398,100,208,114,102,290,208,246,324,782,296,280,796,636,268,84,74,246,34,38,284,612,1090,332,602,378,84,24,256,204,234,26,226,654,60,206,28,160,220,238,38,204,484,206,440,308,206,246,392,314,814,714,200,244,290,258,50,94,252,572,38,284,1050,286,24,252,24,728,46,400,390,330,214,740,368,244,38,252,32,244,252,246,36,94,22,638,296,206,304,32,34,246,240,20,306,340,28,276,226,814,638,278,40,226,50,38,34,42,630,552,252,84,244,252,240,20,198,346,284,290,202,240,300,206,102,214,204,210,430,210,208,144,252,210,240,208,304,224,208,100,354,102,210,764,102,240,210,208,208,102,208,102,208,208,100,102,208,210,100,100,154,268,222,286,256,260,92,642,232,208,262,204,146,100,260,226,146,72,206,38,98,394,1090,348,2602,112,102,490,526,312,486,366,368,368,368,368,674,46,462,202,220,210,516,906,154,384,300,280,206,102,102,102,102,102,102,102,626,102,160,88,226,50,248,34,36,632,308,1124,684,450,254,252,714,60]

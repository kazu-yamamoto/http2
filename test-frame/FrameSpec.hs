{-# LANGUAGE CPP #-}

module FrameSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Network.HTTP2.Frame
import System.FilePath.Glob (compile, globDir)
import Test.Hspec

import JSON

testDir :: FilePath
testDir = "test-frame/http2-frame-test-case"

getTestFiles :: FilePath -> IO [FilePath]
getTestFiles dir =  head <$> globDir [compile "*/*.json"] dir

check :: FilePath -> IO ()
check file = do
    bs <- BL.readFile file
    let etc = eitherDecode bs :: Either String Case
    case etc of
        Left _ -> putStrLn $ "JSON error: " ++ file
        Right tc -> do
            let bin = B16.decodeLenient $ wire tc
                erc = decodeFrame defaultSettings bin
            case erc of
                Left fderr -> case err tc of
                    Nothing -> do
                        putStrLn file -- fixme
                        print fderr
                    Just errs -> do
                        let e = case fderr of
                              FrameDecodeError x _ _ -> x
                        errs `shouldContain` [e]
                Right frm -> do
                    case frame tc of
                        Just fp -> do
                            fpFrame fp `shouldBe` frm
                            let einfo = EncodeInfo {
                                    encodeFlags = flags $ frameHeader $ fpFrame fp
                                  , encodeStreamId = streamId (frameHeader frm)
                                  , encodePadding = unPad <$> fpPad fp
                                  }
                                payload = framePayload frm
                            encodeFrame einfo payload `shouldBe` bin
                        Nothing -> putStrLn file -- fixme

spec :: Spec
spec = do
    describe "decodeFrame and encodeFrame" $ do
        it "decodes test cases well" $ do
            files <- getTestFiles testDir
            forM_ files check

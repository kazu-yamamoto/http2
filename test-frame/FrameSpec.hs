module FrameSpec where


import Control.Monad (forM_)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Hex
import Network.HTTP2
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Test.Hspec

import JSON
import Data.List (isPrefixOf)
import Control.Applicative ((<$>))

testDir :: FilePath
testDir = "test-frame/json"

getTestFiles :: FilePath -> IO [FilePath]
getTestFiles dir = do
    files0 <- getDirectoryContents dir
    let files1 = filter (not . isPrefixOf ".") files0
        files2 = map (dir </>) files1
    return files2

check :: FilePath -> IO ()
check file = do
    bs <- BL.readFile file
    let etc = eitherDecode bs :: Either String Case
    case etc of
        Left _ -> putStrLn $ "JSON error: " ++ file
        Right tc -> do
            let Just bin = unhex $ wire tc
                erc = decodeFrame defaultSettings bin
            case erc of
                Left e -> do
                    let Just errs = err tc
                    errs `shouldContain` [fromErrorCodeId e]
                Right frm -> do
                    let Just fp = frame tc
                    fpFrame fp `shouldBe` frm
                    let einfo = EncodeInfo {
                            encodeFlags = 0
                          , encodeStreamId = streamId (frameHeader frm)
                          , encodePadding = unPad <$> fpPad fp
                          }
                        payload = framePayload frm
                    encodeFrame einfo payload `shouldBe` bin

spec :: Spec
spec = do
    describe "decodeFrame and encodeFrame" $ do
        it "decodes test cases well" $ do
            files <- getTestFiles testDir
            forM_ files check

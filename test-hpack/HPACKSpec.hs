module HPACKSpec where

import Control.Applicative ((<$>))
import Control.Monad (forM_, filterM)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

import Types
import HPACK

testDir :: FilePath
testDir = "test-hpack/hpack-test-case"

getTestFiles :: FilePath -> IO [FilePath]
getTestFiles dir = do
    subdirs0 <- valid <$> getDirectoryContents dir
    subdirs1 <- filterM doesDirectoryExist subdirs0
    concat <$> mapM getTestFiles' subdirs1
  where
    valid = map (testDir </>) . filter (not . isPrefixOf ".")

getTestFiles' :: FilePath -> IO [FilePath]
getTestFiles' subdir = do
    files0 <- valid <$> getDirectoryContents subdir
    filterM doesFileExist files0
  where
    valid = map (subdir </>) . filter (isSuffixOf ".json")

test :: FilePath -> IO Result
test file = do
    bs <- BL.readFile file
    let etc = eitherDecode bs :: Either String Test
    case etc of
        Left e   -> return $ Fail $ file ++ ": " ++ e
        Right tc -> do
            res <- run tc
            case res of
                Pass   -> return Pass
                Fail e -> return $ Fail $ file ++ ": " ++ e

spec :: Spec
spec = do
    describe "decodeRequestHeader" $ do
        it "decodes headers in request" $ do
            files <- getTestFiles testDir
            forM_ files $ \file ->
                test file `shouldReturn` Pass

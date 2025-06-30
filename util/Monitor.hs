module Monitor (monitor, labelMe) where

import Control.Monad
import Data.List
import Data.Maybe
import GHC.Conc.Sync

monitor :: IO () -> IO ()
monitor action = do
    labelMe "monitor"
    forever $ do
        action
        threadSummary >>= mapM_ (putStrLn . showT)
        putStr "\n"
  where
    showT (i, l, s) = i ++ " " ++ l ++ ": " ++ show s

threadSummary :: IO [(String, String, ThreadStatus)]
threadSummary = listThreads >>= mapM summary . sort
  where
    summary t = do
        let idstr = drop 9 $ show t
        l <- fromMaybe "(no name)" <$> threadLabel t
        s <- threadStatus t
        return (idstr, l, s)

labelMe :: String -> IO ()
labelMe lbl = do
    tid <- myThreadId
    labelThread tid lbl

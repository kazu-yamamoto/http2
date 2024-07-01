module Monitor (monitor) where

import Control.Monad
import Data.List
import Data.Maybe
import GHC.Conc.Sync

monitor :: IO () -> IO ()
monitor action = do
    tid <- myThreadId
    labelThread tid "H2 monitor"
    forever $ do
        action
        threadSummary >>= mapM_ (putStrLn . showT)
        putStr "\n"
  where
    showT (i, l, s) = i ++ " " ++ l ++ ": " ++ show s

threadSummary :: IO [(String, String, ThreadStatus)]
threadSummary = (sort <$> listThreads) >>= mapM summary
  where
    summary t = do
        let idstr = drop 9 $ show t
        l <- fromMaybe "(no name)" <$> threadLabel t
        s <- threadStatus t
        return (idstr, l, s)

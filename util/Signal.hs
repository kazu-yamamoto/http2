{-# LANGUAGE CPP #-}

module Signal (onInterrupt) where

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#else
import System.Posix.Signals
#endif

-- | Running the action on Ctrl-C (and SIGTERM on Unix) instead of
--   having the main thread interrupted.
onInterrupt :: IO () -> IO ()
#if defined(mingw32_HOST_OS)
onInterrupt action = () <$ installHandler (Catch $ const action)
#else
onInterrupt action = mapM_ install [sigINT, sigTERM]
  where
    install sig = installHandler sig (Catch action) Nothing
#endif

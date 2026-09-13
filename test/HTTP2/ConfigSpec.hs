module HTTP2.ConfigSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import qualified Network.Socket as S
import qualified System.TimeManager as T
import System.Timeout (timeout)
import Test.Hspec

import Network.HTTP2.Server (allocSimpleConfig', confTimeoutManager, freeSimpleConfig)

spec :: Spec
spec = describe "simple configuration timeout ownership (#175)" $ do
    it "runs callbacks while the configuration is live" $
        withPair $ \socket ->
            bracket (allocSimpleConfig' socket 4096 10000) freeSimpleConfig $ \config -> do
                fired <- newEmptyMVar
                T.withHandle (confTimeoutManager config) (putMVar fired ()) $ \_ ->
                    timeout 2000000 (takeMVar fired) `shouldReturn` Just ()

    it "cancels callbacks when their handle scope ends before release" $
        withPair $ \socket -> do
            fired <- newEmptyMVar
            bracket (allocSimpleConfig' socket 4096 1000000) freeSimpleConfig $ \config ->
                T.withHandle (confTimeoutManager config) (putMVar fired ()) $ \_ -> pure ()
            timeout 3000000 (takeMVar fired) `shouldReturn` Nothing

    -- Regression for https://github.com/kazu-yamamoto/http2/issues/175.
    -- This records the earlier cleanup expectation for discussion: it is not
    -- a claim that HTTP2 currently documents ownership of external handles.
    -- The live-manager control checks callback delivery. Absence over three
    -- seconds is bounded evidence; delivery after release directly disproves it.
    it "cancels registered callbacks when freeSimpleConfig releases the configuration" $
        withPair $ \socket -> do
            fired <- newEmptyMVar
            handle <- bracket (allocSimpleConfig' socket 4096 1000000) freeSimpleConfig $ \config ->
                T.register (confTimeoutManager config) (putMVar fired ())
            (timeout 3000000 (takeMVar fired) `shouldReturn` Nothing)
                `finally` T.cancel handle

withPair :: (S.Socket -> IO a) -> IO a
withPair action = S.withSocketsDo $
    bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \listener -> do
        S.bind listener (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, 1)))
        S.listen listener 1
        port <- S.socketPort listener
        bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \client -> do
            S.connect client (S.SockAddrInet port (S.tupleToHostAddress (127, 0, 0, 1)))
            bracket (fst <$> S.accept listener) S.close action

{-# LANGUAGE OverloadedLabels #-}
module UnliftTests (unliftTests) where

import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit
import qualified UnliftIO.Async as A

import Effectful
import qualified Utils as U

unliftTests :: TestTree
unliftTests = testGroup "Unlift"
  [ testCase "Reset strategy in new thread" test_resetStrategy
  , testCase "SeqUnlift in new thread" test_seqUnliftInNewThread
  , testGroup "Ephemeral strategy"
    [ testCase "Invalid limit" test_ephemeralInvalid
    , testCase "Uses in same thread" test_ephemeralSameThread
    , testCase "Uses in multiple threads" test_ephemeralMultipleThreads
    ]
  , testGroup "Persistent strategy"
    [ testCase "Invalid limit" test_persistentInvalid
    , testCase "Uses in same thread" test_persistentSameThread
    , testCase "Uses in multiple threads" test_persistentMultipleThreads
    ]
  ]

test_resetStrategy :: Assertion
test_resetStrategy = runEff $ do
  s <- withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ do
    withEffToIO $ \runInIO -> do
      inThread $ runInIO unliftStrategy
  U.assertEqual "correct strategy" SeqUnlift s

test_seqUnliftInNewThread :: Assertion
test_seqUnliftInNewThread = runEff $ do
  assertThrowsUnliftError "InvalidUseOfSeqUnlift error"
    (has #_InvalidUseOfSeqUnlift) $ do
      withUnliftStrategy SeqUnlift $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()

test_ephemeralInvalid :: Assertion
test_ephemeralInvalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfUses error"
    (has #_InvalidNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 0) $ do
        withEffToIO $ \_ -> return ()

test_ephemeralSameThread :: Assertion
test_ephemeralSameThread = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error"
    (has #_ExceededNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 1) $ do
        withEffToIO $ \runInIO -> inThread $ do
          runInIO $ return ()
          runInIO $ return ()

test_ephemeralMultipleThreads :: Assertion
test_ephemeralMultipleThreads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error"
    (has #_ExceededNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 1) $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()
          inThread $ runInIO $ return ()

test_persistentInvalid :: Assertion
test_persistentInvalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfThreads error"
    (has #_InvalidNumberOfThreads) $ do
      withUnliftStrategy (ConcUnlift Persistent $ Limited 0) $ do
        withEffToIO $ \_ -> return ()

test_persistentSameThread :: Assertion
test_persistentSameThread = runEff $ do
  withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
    withEffToIO $ \runInIO -> inThread $ do
      runInIO $ return ()
      runInIO $ return ()

test_persistentMultipleThreads :: Assertion
test_persistentMultipleThreads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfThreads error"
    (has #_ExceededNumberOfThreads) $ do
      withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()
          inThread $ runInIO $ return ()

----------------------------------------
-- Helpers

assertThrowsUnliftError
  :: IOE :> es
  => String -> (UnliftError -> Bool) -> Eff es a -> Eff es ()
assertThrowsUnliftError = U.assertThrows

inThread :: IO a -> IO a
inThread k = A.async k >>= A.wait

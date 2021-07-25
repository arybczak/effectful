{-# LANGUAGE CPP #-}
module Concurrency (concurrencyBenchmark) where

#ifdef VERSION_criterion
import Criterion hiding (env)
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench hiding (env)
#endif

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class

import Effectful.Handler
import Effectful.Monad
import Utils

concurrencyBenchmark :: Benchmark
concurrencyBenchmark = bgroup "concurrency"
  [ bgroup "shallow" $ map shallowBench [1, 10, 100]
  , bgroup "deep" $ map deepBench [1, 10, 100]
  ]

shallowBench :: Int -> Benchmark
shallowBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (IO)" $ nfAppIO (asyncBenchIO n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runShallow . runFork1 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runShallow . runFork3 . forkBench n) op
  ]

deepBench :: Int -> Benchmark
deepBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runDeep . runFork1 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runDeep . runFork3 . forkBench n) op
  ]

op :: Monad m => m Int
op = pure 1

----------------------------------------

data Fork :: Effect where
  ForkWithUnmask :: ((forall a. m a -> m a) -> m r) -> Fork m (Async r)

-- | Uses 'localUnliftIO' and 'withLiftMapIO'.
runFork1 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork1 = interpretM $ \env -> \case
  ForkWithUnmask m -> withLiftMapIO $ \liftMap -> do
    localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask

-- | Uses 'localLiftUnliftIO'.
runFork3 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork3 = interpretM $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnliftIO env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

----------------------------------------

asyncBenchIO :: Int -> IO Int -> IO Int
asyncBenchIO n f = do
  a <- asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  wait a
{-# NOINLINE asyncBenchIO #-}

forkBench :: (IOE :> es, Fork :> es) => Int -> Eff es Int -> Eff es Int
forkBench n f = do
  a <- send $ ForkWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  liftIO $ wait a
{-# NOINLINE forkBench #-}

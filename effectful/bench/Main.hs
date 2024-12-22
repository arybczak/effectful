{-# LANGUAGE CPP #-}
module Main (main) where

#ifdef VERSION_criterion
import Criterion
import Criterion.Main
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench
#endif

import Concurrency
import Countdown
import FileSizes
import Unlift

----------------------------------------

import Data.Foldable
import Effectful
import Effectful.Output.Dynamic

benchOutput
  :: (forall r es. Eff (Output Int : es) r -> Eff es (r, x))
  -> Int
  -> IO x
benchOutput run n = fmap snd . runEff . run $ forM_ [1..n] output

main :: IO ()
main = defaultMain
  [ bgroup "output"
    [ bench "array" $ nfAppIO (benchOutput runOutputLocalArray) 1000000
    , bench "list" $ nfAppIO (benchOutput runOutputLocalList) 1000000
    ]
  , concurrencyBenchmark
  , unliftBenchmark
  , bgroup "countdown" $ map countdown [1000, 2000, 3000]
  , bgroup "countdown (extra)" $ map countdownExtra [1000, 2000, 3000]
  , bgroup "filesize" $ map filesize  [1000, 2000, 3000]
  ]

countdownExtra :: Integer -> Benchmark
countdownExtra n = bgroup (show n)
  [ bgroup "effectful (local/static/state)"
    [ bench "shallow" $ nf countdownEffectfulLocalSt n
    , bench "deep"    $ nf countdownEffectfulLocalDeepSt n
    ]
  , bgroup "effectful (local/static/stateM)"
    [ bench "shallow" $ nf countdownEffectfulLocalStM n
    , bench "deep"    $ nf countdownEffectfulLocalDeepStM n
    ]
  , bgroup "effectful (local/dynamic/labeled)"
    [ bench "shallow" $ nf countdownEffectfulLabeledDynLocal n
    , bench "deep"    $ nf countdownEffectfulLabeledDynLocalDeep n
    ]
  , bgroup "effectful (shared/dynamic/labeled)"
    [ bench "shallow" $ nf countdownEffectfulLabeledDynShared n
    , bench "deep"    $ nf countdownEffectfulLabeledDynSharedDeep n
    ]
  , bgroup "effectful (local/dynamic/double)"
    [ bench "shallow" $ nf countdownEffectfulDoubleDynLocal n
    , bench "deep"    $ nf countdownEffectfulDoubleDynLocalDeep n
    ]
  , bgroup "effectful (shared/dynamic/double)"
    [ bench "shallow" $ nf countdownEffectfulDoubleDynShared n
    , bench "deep"    $ nf countdownEffectfulDoubleDynSharedDeep n
    ]
  ]

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bench "reference (pure)" $ nf countdownRef n
  , bench "reference (ST)"   $ nf countdownST n
  , bgroup "effectful (local/static)"
    [ bench "shallow" $ nf countdownEffectfulLocal n
    , bench "deep"    $ nf countdownEffectfulLocalDeep n
    ]
  , bgroup "effectful (local/dynamic)"
    [ bench "shallow" $ nf countdownEffectfulDynLocal n
    , bench "deep"    $ nf countdownEffectfulDynLocalDeep n
    ]
  , bgroup "effectful (local/dynamic/labeled/send)"
    [ bench "shallow" $ nf countdownEffectfulLabeledDynSendLocal n
    , bench "deep"    $ nf countdownEffectfulLabeledDynSendLocalDeep n
    ]
  , bgroup "effectful (shared/static)"
    [ bench "shallow" $ nf countdownEffectfulShared n
    , bench "deep"    $ nf countdownEffectfulSharedDeep n
    ]
  , bgroup "effectful (shared/dynamic)"
    [ bench "shallow" $ nf countdownEffectfulDynShared n
    , bench "deep"    $ nf countdownEffectfulDynSharedDeep n
    ]
  , bgroup "effectful (shared/dynamic/labeled/send)"
    [ bench "shallow" $ nf countdownEffectfulLabeledDynSendShared n
    , bench "deep"    $ nf countdownEffectfulLabeledDynSendSharedDeep n
    ]
#ifdef VERSION_cleff
  , bgroup "cleff (local)"
    [ bench "shallow" $ nf countdownCleffLocal n
    , bench "deep"    $ nf countdownCleffLocalDeep n
    ]
  , bgroup "cleff (IORef)"
    [ bench "shallow" $ nf countdownCleffIORef n
    , bench "deep"    $ nf countdownCleffIORefDeep n
    ]
#endif
#ifdef VERSION_freer_simple
  , bgroup "freer-simple"
    [ bench "shallow" $ nf countdownFreerSimple n
    , bench "deep"    $ nf countdownFreerSimpleDeep n
    ]
#endif
#ifdef VERSION_eff
  , bgroup "eff"
    [ bench "shallow" $ nf countdownEff n
    , bench "deep"    $ nf countdownEffDeep n
    ]
#endif
#ifdef VERSION_mtl
  , bgroup "mtl"
    [ bench "shallow" $ nf countdownMtl n
    , bench "deep"    $ nf countdownMtlDeep n
    ]
#endif
#ifdef VERSION_fused_effects
  , bgroup "fused-effects"
    [ bench "shallow" $ nf countdownFusedEffects n
    , bench "deep"    $ nf countdownFusedEffectsDeep n
    ]
#endif
#ifdef VERSION_polysemy
  , bgroup "polysemy"
    [ bench "shallow" $ nf countdownPolysemy n
    , bench "deep"    $ nf countdownPolysemyDeep n
    ]
#endif
  ]

filesize :: Int -> Benchmark
filesize n = bgroup (show n)
  [ bench "reference" $ nfAppIO ref_calculateFileSizes (take n files)
  , bgroup "effectful"
    [ bench "shallow" $ nfAppIO effectful_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO effectful_calculateFileSizesDeep (take n files)
    ]
#ifdef VERSION_cleff
  , bgroup "cleff"
    [ bench "shallow" $ nfAppIO cleff_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO cleff_calculateFileSizesDeep (take n files)
    ]
#endif
#ifdef VERSION_freer_simple
  , bgroup "freer-simple"
    [ bench "shallow" $ nfAppIO fs_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO fs_calculateFileSizesDeep (take n files)
    ]
#endif
#ifdef VERSION_eff
  , bgroup "eff"
    [ bench "shallow" $ nfAppIO eff_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO eff_calculateFileSizesDeep (take n files)
    ]
#endif
#ifdef VERSION_mtl
  , bgroup "mtl"
    [ bench "shallow" $ nfAppIO mtl_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO mtl_calculateFileSizesDeep (take n files)
    ]
#endif
#ifdef VERSION_fused_effects
  , bgroup "fused-effects"
    [ bench "shallow" $ nfAppIO fe_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO fe_calculateFileSizesDeep (take n files)
    ]
#endif
#ifdef VERSION_polysemy
  , bgroup "polysemy"
    [ bench "shallow" $ nfAppIO poly_calculateFileSizes (take n files)
    , bench "deep"    $ nfAppIO poly_calculateFileSizesDeep (take n files)
    ]
#endif
  ]
  where
    files :: [FilePath]
    files = repeat "effectful.cabal"

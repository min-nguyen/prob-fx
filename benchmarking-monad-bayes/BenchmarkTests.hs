{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use camelCase" #-}
module BenchmarkTests where

import BenchmarkPrograms

import Criterion.Main
import Criterion.Types
import Data.Bifunctor
import Control.DeepSeq

configFile = defaultConfig {csvFile = Just "monad-bayes-benchmarks.csv"}

{- log Regression -}

benchmark :: forall a. NFData a
  => String
  -> (Int -> Int -> IO a)
  -> [(String, (Int, Int))]
  -> IO ()
benchmark groupName benchmarkProg params = do
  defaultMainWith configFile
    [bgroup groupName
      [ bench label (nfIO $  (benchmarkProg sample_size data_size ))
      | (label, (sample_size, data_size)) <- params  ]
    ]

{- Varying over sample size -}

benchmarkLinRegrSim_SampleSize = do
    let data_size = 100
    benchmark "linRegr/Sim/sample-size" simLinRegr
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkLinRegrLW_SampleSize = do
    let data_size = 100
    benchmark "linRegr/LW/sample-size" lwLinRegr
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkLinRegrMH_SampleSize = do
    let data_size = 100
    benchmark "linRegr/MH/sample-size" mhLinRegr
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkHMMSim_SampleSize = do
    let data_size = 100
    benchmark "hmm/Sim/sample-size" simHMM
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkHMMLW_SampleSize = do
    let data_size = 100
    benchmark "hmm/LW/sample-size" lwHMM
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkHMMMH_SampleSize = do
    let data_size = 100
    benchmark "hmm/MH/sample-size" mhHMM
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkTopicSim_SampleSize = do
    let data_size = 100
    benchmark "lda/Sim/sample-size" simLDA
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkTopicLW_SampleSize = do
    let data_size = 100
    benchmark "lda/LW/sample-size" lwLDA
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

benchmarkTopicMH_SampleSize = do
    let data_size = 100
    benchmark "lda/MH/sample-size" mhLDA
       [(show sample_size, (sample_size, data_size)) | sample_size <- [2000, 4000, 6000, 8000, 10000]]

{- Varying over dataset size -}

benchmarkLinRegrSim_DataSize = do
    let sample_size = 2000
    benchmark "linRegr/Sim/data-size" simLinRegr
       [(show data_size, (sample_size, data_size)) | data_size <- [200, 400, 600, 800, 1000]]

benchmarkLinRegrLW_DataSize = do
    let sample_size = 2000
    benchmark "linRegr/LW/data-size" lwLinRegr
       [(show data_size, (sample_size, data_size)) | data_size <- [200, 400, 600, 800, 1000]]

benchmarkLinRegrMH_DataSize = do
    let sample_size = 2000
    benchmark "linRegr/MH/data-size" mhLinRegr
       [(show data_size, (sample_size, data_size)) | data_size <- [200, 400, 600, 800, 1000]]

benchmarkHMMSim_DataSize = do
    let sample_size = 2000
    benchmark "hmm/Sim/data-size" simHMM
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkHMMLW_DataSize = do
    let sample_size = 2000
    benchmark "hmm/LW/data-size" lwHMM
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkHMMMH_DataSize = do
    let sample_size = 2000
    benchmark "hmm/MH/data-size" mhHMM
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkTopicSim_DataSize = do
    let sample_size = 2000
    benchmark "lda/Sim/data-size" simLDA
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkTopicLW_DataSize = do
    let sample_size = 2000
    benchmark "lda/LW/data-size" lwLDA
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkTopicMH_DataSize = do
    let sample_size = 2000
    benchmark "lda/MH/data-size" mhLDA
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]


{-
  benchmarking simHmm/20000  , 10
  time                 240.5 ms   (227.7 ms .. 255.7 ms)
                      0.997 R²   (0.992 R² .. 1.000 R²)
  mean                 231.9 ms   (228.9 ms .. 238.8 ms)
  std dev              5.631 ms   (1.519 ms .. 8.297 ms)
  variance introduced by outliers: 14% (moderately inflated)

  benchmarking simHmm/40000  , 10
  time                 479.0 ms   (451.6 ms .. 501.6 ms)
                      1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 469.6 ms   (461.5 ms .. 475.6 ms)
  std dev              8.235 ms   (4.185 ms .. 11.35 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simHmm/60000  , 10
  time                 690.0 ms   (683.0 ms .. 698.4 ms)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 682.6 ms   (673.4 ms .. 685.9 ms)
  std dev              6.229 ms   (295.2 μs .. 7.565 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simHmm/80000  , 10
  time                 942.2 ms   (902.6 ms .. 964.7 ms)
                      1.000 R²   (1.000 R² .. NaN R²)
  mean                 921.8 ms   (913.7 ms .. 930.5 ms)
  std dev              10.66 ms   (4.970 ms .. 14.40 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simHmm/100000  , 10
  time                 1.157 s    (1.111 s .. 1.196 s)
                      1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 1.152 s    (1.136 s .. 1.158 s)
  std dev              11.08 ms   (539.5 μs .. 14.63 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}

{- LW runs faster than simulation because there is a lot less sampling
  benchmarking lwHmm/20000  , 10
  time                 103.1 ms   (100.3 ms .. 105.8 ms)
                      0.999 R²   (0.998 R² .. 1.000 R²)
  mean                 103.0 ms   (101.8 ms .. 104.2 ms)
  std dev              1.885 ms   (1.371 ms .. 2.647 ms)

  benchmarking lwHmm/40000  , 10
  time                 200.7 ms   (189.3 ms .. 209.8 ms)
                      0.997 R²   (0.992 R² .. 1.000 R²)
  mean                 201.4 ms   (197.2 ms .. 207.0 ms)
  std dev              6.598 ms   (3.723 ms .. 9.508 ms)
  variance introduced by outliers: 14% (moderately inflated)

  benchmarking lwHmm/60000  , 10
  time                 277.5 ms   (223.3 ms .. 326.5 ms)
                      0.990 R²   (0.982 R² .. 1.000 R²)
  mean                 307.5 ms   (286.7 ms .. 361.5 ms)
  std dev              40.78 ms   (2.161 ms .. 56.82 ms)
  variance introduced by outliers: 37% (moderately inflated)

  benchmarking lwHmm/80000  , 10
  time                 366.2 ms   (353.5 ms .. 384.6 ms)
                      1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 400.2 ms   (386.3 ms .. 424.1 ms)
  std dev              23.21 ms   (4.686 ms .. 30.30 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking lwHmm/100000  , 10
  time                 468.1 ms   (311.6 ms .. 578.1 ms)
                      0.988 R²   (0.957 R² .. 1.000 R²)
  mean                 500.9 ms   (475.4 ms .. 524.0 ms)
  std dev              32.13 ms   (12.69 ms .. 44.35 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}

{-
  benchmarking mhHmm/20000  , 10
  time                 899.6 ms   (850.8 ms .. 952.8 ms)
                      1.000 R²   (0.998 R² .. 1.000 R²)
  mean                 916.3 ms   (901.8 ms .. 927.5 ms)
  std dev              14.66 ms   (6.808 ms .. 19.85 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhHmm/40000  , 10
  time                 1.824 s    (1.812 s .. 1.830 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.822 s    (1.819 s .. 1.823 s)
  std dev              2.713 ms   (1.287 ms .. 3.440 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhHmm/60000  , 10
  time                 2.742 s    (2.677 s .. 2.802 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.739 s    (2.730 s .. 2.748 s)
  std dev              11.35 ms   (3.431 ms .. 14.24 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhHmm/80000  , 10
  time                 3.613 s    (3.435 s .. 3.760 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 3.643 s    (3.609 s .. 3.672 s)
  std dev              39.28 ms   (33.73 ms .. 43.79 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhHmm/100000  , 10
  time                 4.558 s    (4.416 s .. 4.735 s)
                      1.000 R²   (NaN R² .. 1.000 R²)
  mean                 4.581 s    (4.538 s .. 4.643 s)
  std dev              58.53 ms   (1.375 ms .. 72.23 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}


{-
  benchmarking simTopic/20000  , 10
  time                 350.2 ms   (326.0 ms .. 373.4 ms)
                      0.999 R²   (0.998 R² .. 1.000 R²)
  mean                 353.2 ms   (349.7 ms .. 356.7 ms)
  std dev              4.112 ms   (3.467 ms .. 4.572 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simTopic/40000  , 10
  time                 686.8 ms   (663.7 ms .. 727.5 ms)
                      1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 703.5 ms   (694.6 ms .. 710.0 ms)
  std dev              9.683 ms   (4.760 ms .. 13.60 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simTopic/60000  , 10
  time                 1.027 s    (1.018 s .. 1.041 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.075 s    (1.056 s .. 1.105 s)
  std dev              28.57 ms   (9.074 ms .. 37.56 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simTopic/80000  , 10
  time                 1.387 s    (1.350 s .. 1.428 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.408 s    (1.395 s .. 1.429 s)
  std dev              19.90 ms   (1.341 ms .. 24.99 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking simTopic/100000 , 10
  time                 1.717 s    (1.660 s .. 1.757 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.764 s    (1.738 s .. 1.788 s)
  std dev              29.32 ms   (16.93 ms .. 35.80 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}

{-
  benchmarking lwTopic/20000
  time                 933.2 ms   (855.5 ms .. 1.043 s)
                      0.998 R²   (0.995 R² .. 1.000 R²)
  mean                 947.5 ms   (926.1 ms .. 961.1 ms)
  std dev              22.25 ms   (11.39 ms .. 31.28 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking lwTopic/40000
  time                 1.880 s    (1.721 s .. 2.030 s)
                      0.999 R²   (0.997 R² .. 1.000 R²)
  mean                 1.940 s    (1.894 s .. 2.013 s)
  std dev              70.08 ms   (13.48 ms .. 92.20 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking lwTopic/60000
  time                 2.761 s    (2.689 s .. NaN s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.866 s    (2.815 s .. 2.911 s)
  std dev              60.45 ms   (26.15 ms .. 74.33 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking lwTopic/80000
  time                 3.712 s    (3.579 s .. 3.846 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 3.907 s    (3.828 s .. 3.990 s)
  std dev              102.8 ms   (46.37 ms .. 138.4 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking lwTopic/100000
  time                 4.719 s    (4.360 s .. 5.085 s)
                      0.999 R²   (0.998 R² .. 1.000 R²)
  mean                 4.766 s    (4.660 s .. 4.866 s)
  std dev              114.1 ms   (62.40 ms .. 158.6 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}

{-
  benchmarking mhTopic/20000
  time                 2.319 s    (2.296 s .. 2.367 s)
                      1.000 R²   (1.000 R² .. NaN R²)
  mean                 2.356 s    (2.337 s .. 2.387 s)
  std dev              29.61 ms   (4.916 ms .. 38.23 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhTopic/40000
  time                 4.633 s    (4.509 s .. 4.725 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 4.728 s    (4.682 s .. 4.815 s)
  std dev              84.45 ms   (700.2 μs .. 100.4 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhTopic/60000
  time                 7.164 s    (6.802 s .. 7.667 s)
                      0.999 R²   (0.998 R² .. NaN R²)
  mean                 7.140 s    (7.038 s .. 7.179 s)
  std dev              69.46 ms   (6.624 ms .. 90.68 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhTopic/80000
  time                 9.491 s    (9.418 s .. 9.568 s)
                      1.000 R²   (NaN R² .. 1.000 R²)
  mean                 9.447 s    (9.423 s .. 9.469 s)
  std dev              26.37 ms   (22.60 ms .. 28.09 ms)
  variance introduced by outliers: 19% (moderately inflated)

  benchmarking mhTopic/100000
  time                 11.91 s    (11.53 s .. 12.39 s)
                      1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 11.80 s    (11.70 s .. 11.87 s)
  std dev              98.44 ms   (56.84 ms .. 122.8 ms)
  variance introduced by outliers: 19% (moderately inflated)
-}
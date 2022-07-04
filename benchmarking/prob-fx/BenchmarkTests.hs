{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module BenchmarkTests where

import qualified Data.Map as Map
import BenchmarkPrograms
import qualified Freer.Inference.SIM as SIM
import qualified Freer.Inference.LW as LW
import qualified Freer.Inference.MH as MH
import Freer.Effects.State
import Freer.Model
import Sampler
import Freer.Effects.ObsReader
import Env
import Util
import Debug.Trace
import Unsafe.Coerce
import Freer.Trace
import Criterion.Main
import Criterion.Types
import Control.DeepSeq

configFile = defaultConfig {csvFile = Just "benchmarking/prob-fx/prob-fx-benchmarks.csv"}

benchmark :: forall a. NFData a
  => String                     -- benchmark group name
  -> (Int -> Int -> Sampler a)  -- sample size -> dataset size -> Sampler computation
  -> [(String, (Int, Int))]     -- [(benchmark label, (sample size, dataset set))]
  -> IO ()
benchmark groupName benchmarkProg params = do
  defaultMainWith configFile
    [bgroup groupName
      [ bench label (nfIO $ sampleIOFixed (benchmarkProg sample_size data_size    ))
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

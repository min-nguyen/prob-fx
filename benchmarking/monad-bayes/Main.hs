module Main where

import BenchmarkPrograms
import BenchmarkTests
import System.Environment (getArgs)

main :: IO ()
main = do
 benchSampleSize
 benchDataSize

benchSampleSize = do
  benchmarkLinRegrSim_SampleSize
  benchmarkLinRegrLW_SampleSize
  benchmarkLinRegrMH_SampleSize
  benchmarkHMMSim_SampleSize
  benchmarkHMMLW_SampleSize
  benchmarkHMMMH_SampleSize
  benchmarkTopicSim_SampleSize
  benchmarkTopicLW_SampleSize
  benchmarkTopicMH_SampleSize

benchDataSize = do
  benchmarkLinRegrSim_DataSize
  benchmarkLinRegrLW_DataSize
  benchmarkLinRegrMH_DataSize
  benchmarkHMMSim_DataSize
  benchmarkHMMLW_DataSize
  benchmarkHMMMH_DataSize
  benchmarkTopicSim_DataSize
  benchmarkTopicLW_DataSize
  benchmarkTopicMH_DataSize

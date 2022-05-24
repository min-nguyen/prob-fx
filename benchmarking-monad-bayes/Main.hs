module Main where

import BenchmarkPrograms
import BenchmarkTests
import System.Environment (getArgs)

main :: IO ()
main = do
 benchSampleSize
 benchDataSize

benchSampleSize = do
  benchmarkLogRegrSim_SampleSize
  benchmarkLogRegrLW_SampleSize
  benchmarkLogRegrMH_SampleSize
  benchmarkHMMSim_SampleSize
  benchmarkHMMLW_SampleSize
  benchmarkHMMMH_SampleSize
  benchmarkTopicSim_SampleSize
  benchmarkTopicLW_SampleSize
  benchmarkTopicMH_SampleSize

benchDataSize = do
  benchmarkLogRegrSim_DataSize
  benchmarkLogRegrLW_DataSize
  benchmarkLogRegrMH_DataSize
  benchmarkHMMSim_DataSize
  benchmarkHMMLW_DataSize
  benchmarkHMMMH_DataSize
  benchmarkTopicSim_DataSize
  benchmarkTopicLW_DataSize
  benchmarkTopicMH_DataSize

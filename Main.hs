{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Examples.LinRegr
import Examples.LogRegr
import Examples.SIR
import Examples.LDA
import Examples.Radon
import Examples.School
import Sampler
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

availableCommands = "[simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon, mhSchool]"

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegr"  -> sampleIO simulateLinRegr >>= printThenWrite
  "lwLinRegr"   -> sampleIO inferLwLinRegr >>= printThenWrite
  "simSIR"      -> sampleIO simulateSIR >>= printThenWrite
  "simSIRS"     -> sampleIO simulateSIRS >>= printThenWrite
  "simSIRSV"    -> sampleIO simulateSIRSV >>= printThenWrite
  "mhSIR"       -> sampleIO inferSIR >>= printThenWrite

  "mhLinRegr"   -> sampleIO inferMhLinRegr >>= printThenWrite
  "simLogRegr"  -> sampleIO simulateLogRegr >>= printThenWrite
  "lwLogRegr"   -> sampleIO inferLwLogRegr >>= printThenWrite
  "mhLogRegr"   -> sampleIO inferMHLogRegr >>= printThenWrite
  "simLDA"      -> sampleIO simLDA >>= printThenWrite
  "mhLDA"       -> sampleIO mhLDA >>= printThenWrite
  "simRadon"    -> sampleIO simRadon >>= printThenWrite
  "mhRadon"     -> sampleIO mhRadon >>= printThenWrite
  "mhSchool"    -> sampleIO mhSchool >>= printThenWrite
  _             -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"
                           ++ "available commands: " ++ availableCommands

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX. Available arguments: " ++ availableCommands
               (a:as)  -> parseArgs a

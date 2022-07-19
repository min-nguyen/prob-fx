{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import LinRegr ( simulateLinRegr, inferLwLinRegr, inferMhLinRegr )
import LogRegr ( simulateLogRegr, inferLwLogRegr, inferMHLogRegr )
import SIR ( simulateSIR, inferSIR, simulateSIRS, simulateSIRSV )
import LDA ( simLDA, mhLDA )
import Radon ( simRadon, mhRadon )
import School ( mhSchool )
import Sampler ( sampleIOFixed )
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

availableCommands = "[simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon, mhSchool]"

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegr"  -> sampleIOFixed simulateLinRegr >>= printThenWrite
  "lwLinRegr"   -> sampleIOFixed inferLwLinRegr >>= printThenWrite
  "simSIR"      -> sampleIOFixed simulateSIR >>= printThenWrite
  "simSIRS"     -> sampleIOFixed simulateSIRS >>= printThenWrite
  "simSIRSV"    -> sampleIOFixed simulateSIRSV >>= printThenWrite
  "mhSIR"       -> sampleIOFixed inferSIR >>= printThenWrite

  "mhLinRegr"   -> sampleIOFixed inferMhLinRegr >>= printThenWrite
  "simLogRegr"  -> sampleIOFixed simulateLogRegr >>= printThenWrite
  "lwLogRegr"   -> sampleIOFixed inferLwLogRegr >>= printThenWrite
  "mhLogRegr"   -> sampleIOFixed inferMHLogRegr >>= printThenWrite
  "simLDA"      -> sampleIOFixed simLDA >>= printThenWrite
  "mhLDA"       -> sampleIOFixed mhLDA >>= printThenWrite
  "simRadon"    -> sampleIOFixed simRadon >>= printThenWrite
  "mhRadon"     -> sampleIOFixed mhRadon >>= printThenWrite
  "mhSchool"    -> sampleIOFixed mhSchool >>= printThenWrite
  _             -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"
                           ++ "available commands: " ++ availableCommands

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX. Available arguments: " ++ availableCommands
               (a:as)  -> parseArgs a

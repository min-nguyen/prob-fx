module Main (main) where

import SIR
import LinRegr
import LDA
import Test.Expected
import Test.HUnit
import Sampler
import System.Exit

testSimLinRegr :: Test
testSimLinRegr = TestCase $ do
  output <- sampleIOFixed simulateLinRegr
  assertEqual "Testing simulateLinRegr"  simLinRegrExpected output

testLwLinRegr :: Test
testLwLinRegr = TestCase $ do
  output <- sampleIOFixed inferLwLinRegr
  assertEqual "Testing inferLwLinRegr"  lwLinRegrExpected output

testMhLinRegr :: Test
testMhLinRegr = TestCase $ do
  output <- sampleIOFixed inferMhLinRegr
  assertEqual "Testing inferMhLinRegr"  mhLinRegrExpected output

testSimSIR :: Test
testSimSIR = TestCase $ do
  output <- sampleIOFixed simulateSIR
  assertEqual "Testing simulateSIR"  simSIRExpected output

testMhSIR :: Test
testMhSIR = TestCase $ do
  output <- sampleIOFixed inferSIR
  assertEqual "Testing inferSIR"  mhSIRExpected output

testSimLDA :: Test
testSimLDA = TestCase $ do
  output <- sampleIOFixed simLDA
  assertEqual "Testing simLDA"  simLDAExpected output

testMhPredLDA :: Test
testMhPredLDA = TestCase $ do
  output <- sampleIOFixed mhLDA
  assertEqual "Testing mhLDA"  mhPredLDAExpected output

tests :: Test
tests = TestList [testSimLinRegr, testLwLinRegr, testMhLinRegr, testSimSIR, testMhSIR, testSimLDA, testMhPredLDA]

main :: IO ()
main = do
  Counts cases tried errors failures <- runTestTT tests
  if errors + failures == 0
    then
      exitSuccess
    else do
      exitWith (ExitFailure 1)
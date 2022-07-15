{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module HMM where

import Model
import Inference.SIM as SIM
import Inference.LW as LW
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util

-- ** (Section 2, Fig 2) HMM Loop 
type HMMEnv =
  '[ "trans_p" ':= Double,
     "obs_p"   ':= Double,
     "y"       ':= Int
   ]

hmmFor :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) =>
  Int -> Int -> Model env ts Int
hmmFor n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  let hmmLoop i x_prev | i < n = do
                            dX <- boolToInt <$> bernoulli' trans_p
                            let x = x_prev + dX
                            binomial x obs_p #y
                            hmmLoop (i - 1) x
                       | otherwise = return x_prev
  hmmLoop 0 x

simulateHMM :: Sampler (Int, Env HMMEnv)
simulateHMM = do
  let x_0 = 0; n = 10
      env = #trans_p := [0.5] <:> #obs_p := [0.8] <:> #y := [] <:> nil
  SIM.simulate (hmmFor n) env 0

inferLwHMM :: Sampler  [(Env HMMEnv, Double)]
inferLwHMM   = do
  let x_0 = 0; n = 10
      env = #trans_p := [] <:> #obs_p := [] <:> #y := [0, 1, 1, 3, 4, 5, 5, 5, 6, 5] <:> nil
  LW.lw 100 (hmmFor n) (x_0, env)

-- ** (Section 2, Fig 3) Modular HMM  
transModel ::  Double -> Int -> Model env ts Int
transModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (x_prev + dX)

obsModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
obsModel observation_p x = do
  y <- binomial x observation_p #y
  return y

hmmNode :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_i <- transModel  transition_p x_prev
  y_i <- obsModel observation_p x_i
  return x_i

hmm :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmm n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (>=>) return (replicate n (hmmNode trans_p obs_p)) x

-- ** (Section 3) Higher-order, generic HMM 
type TransModel env ts params lat   = params -> lat -> Model env ts lat
type ObsModel env ts params lat obs = params -> lat -> Model env ts obs

hmmGen :: Model env ts ps1 -> Model env ts ps2
       -> TransModel env ts ps1 lat -> ObsModel env ts ps2 lat obs
       -> Int -> lat -> Model env ts lat
hmmGen transPrior obsPrior transModel obsModel n x_0 = do
  ps1    <- transPrior
  ps2    <- obsPrior
  let hmmNode x = do
                x' <- transModel ps1 x
                y' <- obsModel ps2 x'
                return x'
  foldl (>=>) return (replicate n hmmNode) x_0

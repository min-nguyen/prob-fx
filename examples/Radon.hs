{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Radon where

import Control.Monad
import Model
import Env
import Sampler
import DataSets
import Inference.SIM as SIM
import Inference.MH as MH
import Util

-- | Hierarchical Linear Regression [https://docs.pymc.io/en/v3/pymc-examples/examples/case_studies/multilevel_modeling.html]
type HLREnv =
  '[ "mu_a" ':= Double, "mu_b" ':= Double, "sigma_a" ':= Double, "sigma_b" ':= Double,
     "a" ':= Double, "b" ':= Double, "log_radon" ':= Double]

radonPrior :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b"] Double
  => Model env es (Double, Double, Double, Double)
radonPrior = do
  mu_a    <- normal 0 10 #mu_a
  sigma_a <- halfNormal 5 #sigma_a
  mu_b    <- normal 0 10 #mu_b
  sigma_b <- halfNormal 5 #sigma_b
  return (mu_a, sigma_a, mu_b, sigma_b)

-- n counties = 85, len(floor_x) = 919, len(county_idx) = 919
radonModel :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b", "a", "b", "log_radon"] Double
  => Int -> [Int] -> [Int] -> () -> Model env es [Double]
radonModel n_counties floor_x county_idx _ = do
  (mu_a, sigma_a, mu_b, sigma_b) <- radonPrior
  -- Intercept for each county
  a <- replicateM n_counties (normal mu_a sigma_a #a)  -- length = 85
  -- Gradient for each county
  b <- replicateM n_counties (normal mu_b sigma_b #b)  -- length = 85
  -- Model error
  eps <- halfCauchy' 5
  let -- Get county intercept for each datapoint
      a_county_idx = map (a !!) county_idx
      -- Get county gradient for each datapoint
      b_county_idx = map (b !!) county_idx
      floor_values = map fromIntegral floor_x
      -- Get radon estimate for each data point
      radon_est = zipWith (+) a_county_idx (zipWith (*) b_county_idx floor_values)
  -- Sample radon amount for each data point
  radon_like <- mapM (\rad_est -> normal rad_est eps #log_radon) radon_est
  return radon_like

mkRecordHLR :: ([Double], [Double], [Double], [Double], [Double], [Double], [Double]) -> Env HLREnv
mkRecordHLR (mua, mub, siga, sigb, a, b, lograds) =
   #mu_a := mua <:> #mu_b := mub <:> #sigma_a := siga <:> #sigma_b := sigb <:> #a := a <:> #b := b <:> #log_radon := lograds <:> ENil

simRadon :: Sampler ([Double], [Double])
simRadon = do
  let env_in = mkRecordHLR ([1.45], [-0.68], [0.3], [0.2], [], [], [])
  (bs, env_out) <- SIM.simulate  (radonModel n_counties dataFloorValues countyIdx) env_in  ()
  let basementIdxs      = findIndexes dataFloorValues 0
      noBasementIdxs    = findIndexes dataFloorValues 1
      basementPoints    = map (bs !!) basementIdxs
      nobasementPoints  = map (bs !!) noBasementIdxs
  return (basementPoints, nobasementPoints)

-- Return posterior for intercepts and gradients
mhRadonpost :: Sampler ([Double], [Double])
mhRadonpost = do
  let env_in = mkRecordHLR ([], [], [], [], [], [], logRadon)
  env_outs <- MH.mh 2000 (radonModel n_counties dataFloorValues countyIdx) ((), env_in) ["mu_a", "mu_b", "sigma_a", "sigma_b"]
  let mu_a   = concatMap (get #mu_a)  env_outs
      mu_b   = concatMap (get #mu_b)  env_outs
  return (mu_a, mu_b)

-- Return predictive posterior for intercepts and gradients
mhRadon :: Sampler ([Double], [Double])
mhRadon = do
  let env_in = mkRecordHLR ([], [], [], [], [], [], logRadon)
  env_outs <- MH.mh 1500 (radonModel n_counties dataFloorValues countyIdx) ((), env_in) ["mu_a", "mu_b", "sigma_a", "sigma_b"]
  let env_pred   = head env_outs
      as         = get #a env_pred
      bs         = get #b env_pred
  liftS $ print as
  return (as, bs)
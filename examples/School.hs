
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module School where

import Model
import Inference.MH as MH
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env

-- | Hierarchical School Model
type SchEnv = '[
    "mu"    ':= Double,
    "theta" ':= [Double],
    "y"     ':= Double
  ]

schoolModel :: (Observables env '["mu", "y"] Double, Observable env "theta" [Double])
  => Int -> [Double] -> Model env es [Double]
schoolModel n_schools σs = do
  μ   <- normal 0 10 #mu
  τ   <- halfNormal' 10
  ηs  <- replicateM n_schools (normal' 0 1)
  θs  <- deterministic (map ((μ +) . (τ *)) ηs) #theta
  ys  <- mapM (\(θ, σ) -> normal θ σ #y) (zip θs σs)
  let h = ""
  return θs

-- Perform MH inference
mhSchool :: Sampler ([Double], [[Double]])
mhSchool = do
  let n_schools = 8
      ys        = [28, 8, -3,   7, -1,  1, 18, 12]
      sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
      env       = #mu := [] <:> #theta := [] <:> #y := ys <:> ENil
  env_mh_out <- MH.mh 10000 (schoolModel n_schools ) (sigmas, env) ["mu", "theta"]
  let mus    = concatMap (get #mu) env_mh_out
      thetas = concatMap (get #theta) env_mh_out
  return (mus, thetas)
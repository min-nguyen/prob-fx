
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}

module LinRegr where

import Prog
import Effects.ObsReader
import Effects.Writer
import Model
import Inference.SIM as SIM
import Inference.LW as LW
import Inference.MH as MH
import Effects.Dist
import Effects.Lift
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util

-- ** (Section 1) Linear regression
linRegr :: forall env rs . Member Sample rs =>
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs Double
linRegr x = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  y <- normal (m * x + c) σ #y
  return y

-- ** (Section 1, Fig 1a) SIM from linear regression
simulateLinRegr :: Sampler [(Double, Double)]
simulateLinRegr = do
  let xs  = [0 .. 100]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> nil
  ys_envs <- mapM (SIM.simulate linRegr env) xs
  let ys = map fst ys_envs
  return (zip xs ys)

-- ** (Section 1, Fig 1b) Perform likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
inferLwLinRegr :: Sampler [(Double, Double)]
inferLwLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  lwTrace <- mapM (LW.lw 200 linRegr) xys
  let -- Get output of LW and extract mu samples
      (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  return $ zip mus ps

-- Perform Metropolis-Hastings inference over linear regression
inferMhLinRegr :: Sampler [Double]
inferMhLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  mhTrace <- concat <$> mapM (\xy -> MH.mh 100 linRegr xy ["m", "c"]) xys
  let -- Get output of MH and extract mu samples
      mus = concatMap (get #m) mhTrace
  return mus
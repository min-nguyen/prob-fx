{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

{- | A linear regression model, assuming a linear relationship between x and y co-ordinates.
-}

module LinRegr where

import Model ( Model, normal, uniform )
import Inference.SIM as SIM ( simulate )
import Inference.LW as LW ( lw )
import Inference.MH as MH ( mh )
import Sampler ( Sampler )
import Data.Kind (Constraint)
import Env ( Observables, Observable(get), Assign((:=)), nil, (<:>) )

-- | Linear regression environment
type LinRegrEnv =
    '[  "m" ':= Double, -- ^ gradient
        "σ" ':= Double, -- ^ noise
        "c" ':= Double, -- ^ intercept
        "y" ':= Double  -- ^ output
     ]

-- | Linear regression model
linRegr :: Observables env ["y", "m", "c", "σ"] Double
  -- x co-ordinate
  => Double
  -- y co-ordinate
  -> Model env rs Double
linRegr x = do
  -- Draw prior
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  y <- normal (m * x + c) σ #y
  return y

-- | Simulate from linear regression
simulateLinRegr :: Sampler [(Double, Double)]
simulateLinRegr = do
  -- Specify model inputs
  let xs  = [0 .. 100]
  -- Specify model environment
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> nil
  -- Simulate linear regression for each input x
  ys_envs <- mapM (\x -> SIM.simulate (linRegr x) env) xs
  let ys = map fst ys_envs
  return (zip xs ys)

-- | Likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
inferLwLinRegr :: Sampler [(Double, Double)]
inferLwLinRegr = do
  -- Specify model inputs
  let xs  = [0 .. 100]
  -- Specify model environments and pair with model input
      x_envs = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  -- Run LW for 20 iterations on each pair of model input and environment
  lwTrace <- mapM (\(x, env) -> LW.lw 20 (linRegr x) env) x_envs
   -- Get the sampled values of mu and their likelihood-weighting
  let (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  return $ zip mus ps

-- | Perform Metropolis-Hastings inference over linear regression
inferMhLinRegr :: Sampler [Double]
inferMhLinRegr = do
  -- Specify model inputs
  let xs  = [0 .. 100]
  -- Specify model environments and pair with model input
      x_envs = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  -- Run MH for 100 iterations on each pair of model input and environment
  mhTrace <- concat <$> mapM (\(x, env) -> MH.mh 100 (linRegr x) env ["m", "c"]) x_envs
  -- Get the sampled values of mu
  let mus = concatMap (get #m) mhTrace
  return mus
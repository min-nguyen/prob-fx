{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Examples.LogRegr where

import Control.Monad
import Model
import Env
import Sampler
import Inference.SIM as SIM
import Inference.MH as MH
import Inference.LW as LW

{- | Logistic regression environment.
     This type definition is for readability purposes and is not used anywhere.
-}
type LogRegrEnv =
    '[  "label" ':= Bool,   -- ^ output
        "m" ':= Double,     -- ^ mean
        "b" ':= Double      -- ^ intercept
     ]

-- | Logistic regression model
logRegr
 -- Specify the "observable variables" that may later be provided observed values
 :: (Observable env "label" Bool, Observables env '["m", "b"] Double)
 -- | Model inputs
 => [Double]
 -- | Event occurrences
 -> Model env rs [Bool]
logRegr xs = do
  -- Specify model parameter distributions
  {- Annotating with the observable variable #m lets us later provide observed
     values for m. -}
  m     <- normal 0 5 #m
  b     <- normal 0 1 #b
  {- One can use primed variants of distributions which don't require observable
     variables to be provided. This disables being able to later provide
     observed values to that variable. -}
  sigma <- gamma' 1 1
  -- Specify model output distributions
  ys    <- foldM (\ys x -> do
                    -- probability of event occurring
                    p <- normal' (m * x + b) sigma
                    -- generate as output whether the event occurs
                    y <- bernoulli (sigmoid p) #label
                    return (ys ++ [y])) [] xs
  return ys

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

-- | Simulate from logistic regression
simulateLogRegr :: Sampler [(Double, Bool)]
simulateLogRegr = do
  -- First declare the model inputs
  let xs  = map (/50) [(-50) .. 50]
  -- Define a model environment to simulate from, providing observed values for the model parameters
      env = (#label := []) <:> (#m := [2]) <:> (#b := [-0.15]) <:> nil
  -- Call simulate on logistic regression
  (ys, envs) <- SIM.simulate logRegr env xs
  return (zip xs ys)

-- | Likelihood-weighting over logistic regression
inferLwLogRegr :: Sampler [(Double, Double)]
inferLwLogRegr = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simulateLogRegr
  -- Define environment for inference, providing observed values for the model outputs
  let env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run LW inference for 20000 iterations
  lwTrace <- LW.lw 20000 logRegr (xs, env)
  let -- Get output of LW, extract mu samples, and pair with likelihood-weighting ps
      (env_outs, ps) = unzip lwTrace
      mus = concatMap (get #m) env_outs
  return $ zip mus ps

-- | Metropolis-Hastings inference over logistic regression
inferMHLogRegr :: Sampler [(Double, Double)]
inferMHLogRegr = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simulateLogRegr
  let -- Define an environment for inference, providing observed values for the model outputs
      env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run MH inference for 20000 iterations; the ["m", "b"] is optional for indicating interest in learning #m and #b in particular, causing other variables to not be resampled (unless necessary) during MH.
  mhTrace <- MH.mh 50000 logRegr (xs, env) ["m", "b"]
  -- Retrieve values sampled for #m and #b during MH
  let m_samples = concatMap (get #m) mhTrace
      b_samples = concatMap (get #b) mhTrace
  return (zip m_samples b_samples)
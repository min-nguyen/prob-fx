{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

{- | A logistic regression model, modelling the probability of an event occurring or not.
-}

module LogRegr where

import Control.Monad ( foldM )
import Model ( bernoulli, gamma', normal, normal', Model )
import Env ( (<:>), nil, Assign((:=)), Env, Observable(get), Observables )
import Sampler ( Sampler )
import Inference.SIM as SIM ( simulate )
import Inference.MH as MH ( mh )
import Inference.LW as LW ( lw )

{- | Logistic regression environment.
     This type definition is for readability purposes and is not used anywhere.
-}
type LogRegrEnv =
    '[  "y" ':= Bool,   -- ^ output
        "m" ':= Double, -- ^ mean
        "b" ':= Double  -- ^ intercept
     ]

-- | Logistic regression model
logRegr
 -- Specify the "observable variables" that may later be provided observed values
 :: (Observable env "y" Bool, Observables env '["m", "b"] Double)
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
                    y <- bernoulli (sigmoid p) #y
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
      env = (#y := []) <:> (#m := [2]) <:> (#b := [-0.15]) <:> nil
  -- Call simulate on logistic regression
  (ys, envs) <- SIM.simulate (logRegr xs) env
  return (zip xs ys)

-- | Likelihood-weighting over logistic regression
inferLwLogRegr :: Sampler [(Double, Double)]
inferLwLogRegr = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simulateLogRegr
  -- Define environment for inference, providing observed values for the model outputs
  let env = (#y := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run LW inference for 20000 iterations
  lwTrace :: [(Env LogRegrEnv, Double)] <- LW.lw 20000 (logRegr xs) env
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
      env = (#y := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run MH inference for 20000 iterations
  {- The agument ["m", "b"] is optional for indicating interest in learning #m and #b in particular,
     causing other variables to not be resampled (unless necessary) during MH. -}
  mhTrace :: [Env LogRegrEnv] <- MH.mh 50000 (logRegr xs) env ["m", "b"]
  -- Retrieve values sampled for #m and #b during MH
  let m_samples = concatMap (get #m) mhTrace
      b_samples = concatMap (get #b) mhTrace
  return (zip m_samples b_samples)
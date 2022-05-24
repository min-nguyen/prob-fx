{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Inference.LW where

import qualified Data.Map as Map
import Env
import Effects.ObsReader
import Control.Monad
import Effects.Dist
import Prog
import Model hiding (runModelFree)
import Sampler
import Effects.State ( modify, handleState, State )
import STrace
import Inference.Simulate (traceSamples, handleSamp)

-- | Likelihood Weighting (LW)
lw :: forall env es a b. (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
   => Int                          -- Number of LW iterations
   -> (b -> Model env es a)        -- A model awaiting an input
   -> (b, Env env)                 -- A model input and model environment (containing observed values to condition on)
   -> Sampler [(Env env, Double)]  -- Trace of weighted output environments containing values sampled for each LW iteration
lw n model xs_envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  lwTrace <- runN xs_envs
  return $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- | LW handler
runLW :: es ~ '[ObsReader env, Dist,State STrace,  Observe, Sample]
  => Env env -> Model env es a
  -> Sampler ((a, STrace), Double)
runLW env = handleSamp . handleObs 0 . handleState Map.empty . traceSamples . handleCore env

-- | Accumulate log probabilities of each Observe operation
handleObs :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (a, Double)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)

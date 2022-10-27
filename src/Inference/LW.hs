{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{- | Likelihood-Weighting inference.
-}

module Inference.LW (
    lw
  , runLW
  , handleObs) where

import qualified Data.Map as Map
import Env ( Env )
import Effects.ObsReader ( ObsReader )
import Control.Monad ( replicateM )
import Effects.Dist ( Dist, Observe(..), Sample )
import Prog ( discharge, Member, Prog(..) )
import PrimDist ( logProb )
import Model ( handleCore, Model )
import Sampler ( Sampler )
import Effects.State ( modify, handleState, State )
import Trace ( FromSTrace(..), STrace )
import Inference.SIM (traceSamples, handleSamp)

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lw :: (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
    -- number of LW iterations
    => Int
    -- model awaiting an input
    -> (b -> Model env es a)
    -- (model input, input model environment)
    -> (b, Env env)
    -- [(output model environment, likelihood-weighting)]
    -> Sampler [(Env env, Double)]
lw n model xs_envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  lwTrace <- runN xs_envs
  return $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- | Handler for one iteration of LW
runLW :: es ~ '[ObsReader env, Dist, State STrace, Observe, Sample]
  -- model environment
  => Env env
  -- model
  -> Model env es a
  -- ((model output, sample trace), likelihood-weighting)
  -> Sampler ((a, STrace), Double)
runLW env = handleSamp . handleObs 0 . handleState Map.empty . traceSamples . handleCore env

-- | Handle each @Observe@ operation by computing and accumulating a log probability
handleObs :: Member Sample es
  -- accumulated log-probability
  => Double
  -> Prog (Observe : es) a
  -- (model output, final log-probability)
  -> Prog es (a, Double)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)

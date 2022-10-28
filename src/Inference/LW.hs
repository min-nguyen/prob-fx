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
    => Int            -- ^ number of LW iterations
    -> Model env es a -- ^ model
    -> Env env        -- ^ model environment
    -> Sampler [(Env env, Double)] -- ^ [(output model environment, likelihood-weighting)]
lw n model env = do
  lwTrace <- replicateM n (runLW model env)
  return $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- | Handler for one iteration of LW
runLW :: es ~ '[ObsReader env, Dist, State STrace, Observe, Sample]
  => Model env es a -- ^ model
  -> Env env        -- ^ model environment
  -> Sampler ((a, STrace), Double) -- ^ ((model output, sample trace), likelihood-weighting)
runLW env = handleSamp . handleObs 0 . handleState Map.empty . traceSamples . handleCore env

-- | Handle each @Observe@ operation by computing and accumulating a log probability
handleObs :: Member Sample es
  => Double -- ^ accumulated log-probability
  -> Prog (Observe : es) a
  -> Prog es (a, Double) -- ^ (model output, final log-probability)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)

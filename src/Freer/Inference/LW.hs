{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Freer.Inference.LW where

import qualified Data.Map as Map
import Env
import Freer.Effects.ObsReader
import Control.Monad
import Freer.Effects.Dist
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Freer.Model hiding (runModelFree)
import Sampler
import Freer.Effects.State ( modify, handleState, State )
import Freer.Trace
import Freer.Inference.SIM (traceSamples, handleSamp)

-- | Run LW n times for multiple data points
lw :: forall env es a b. (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
   => Int                                -- Number of lw iterations per data point
   -> (b -> Model env es a)              -- Model awaiting input variable
   -> (b, Env env)                       -- List of model observed variables
   -> Sampler [(Env env, Double)]              -- List of n likelihood weightings for each data point
lw n model xs_envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  lwTrace <- runN xs_envs
  return $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

runLW :: es ~ '[ObsReader env, Dist, State STrace,  Observe, Sample]
  => Env env -> Model env es a
  -> Sampler ((a, STrace), Double)
runLW env = handleSamp . handleObs 0 . handleState Map.empty . traceSamples . handleCore env

handleObs :: Member Sample es => Double -> Eff (Observe : es) a -> Eff es (a, Double)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (E op k) = case decomp op of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (qApp k y)
    Left op' -> E op' (tsingleton $ qComp k (handleObs logp))

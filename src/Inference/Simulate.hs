{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Inference.Simulate where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import Env
import Control.Monad
import Control.Monad.Trans.Class
import Effects.Dist
import Prog
import Model
import Sampler
import Effects.ObsReader
import Effects.State
import Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

-- ||| (Section 6.1) Simulation
simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist,State STrace, Observe, Sample])
  => (b -> Model env es a)  -- A model awaiting an input
  -> Env env                -- A model environment
  -> b                      -- Model input
  -> Sampler (a, Env env)   -- Model output and output environment
simulate model env x  = do
  outputs_strace <- runSimulate env (model x)
  return (fmap fromSTrace outputs_strace)

-- | Simulate handler
runSimulate :: (es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
 => Env env -> Model env es a -> Sampler (a, STrace)
runSimulate env
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore env

-- | Trace sampled values for each Sample operation
traceSamples :: (Member (State STrace) es, Member Sample es) => Prog es a -> Prog es a
traceSamples (Val x) = return x
traceSamples (Op op k) = case prj op of
  Just (Sample (DistDict d) α) ->
       Op op (\x -> do modify (updateSTrace α d x);
                       traceSamples (k x))
  Nothing -> Op op (traceSamples . k)

handleObs :: Prog (Observe : es) a -> Prog es  a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

handleSamp :: Prog '[Sample] a -> Sampler a
handleSamp  (Val x)  = return x
handleSamp  (Op op k) = case discharge op of
  Right (Sample (DistDict d) α) ->
    do  x <- sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"

-- | For simulating many times over many model inputs and environments
simulateMany :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
  => Int                             -- Number of simulations per model input and model environment
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [Env env]                       -- List of model environments
  -> Sampler [(a, Env env)]
simulateMany n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  outputs_smaps <- concat <$> mapM runN (zip xs envs)
  let outputs_envs = map (fmap (fromSTrace @env)) outputs_smaps
  return outputs_envs

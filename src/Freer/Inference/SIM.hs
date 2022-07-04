{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Freer.Inference.SIM where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import Env
import Control.Monad
import Control.Monad.Trans.Class
import Freer.Effects.Dist
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Freer.Model
import Sampler
import Freer.Effects.ObsReader
import Freer.Effects.State
import Freer.Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist,State STrace, Observe, Sample])
  => (b -> Model env es a)
  -> Env env
  -> b
  -> Sampler (a, Env env)
simulate model env x  = do
  outputs_smaps <- runSimulate env (model x)
  let outputs_envs = fmap (fromSTrace @env) outputs_smaps
  return outputs_envs

runSimulate :: (es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
 => Env env -> Model env es a -> Sampler (a, STrace)
runSimulate env
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore env

traceSamples :: (Member (State STrace) es, Member Sample es) => Eff es a -> Eff es a
traceSamples (Val x) = return x
traceSamples (E op k) = case prj op of
  Just (Sample (DistDict d) α) ->
       E op (tsingleton $ \x -> modify  (updateSTrace α d x) >>
                                traceSamples (qApp k x))
  Nothing -> E op (tsingleton $ qComp k traceSamples)

handleObs :: Eff (Observe : es) a -> Eff es  a
handleObs (Val x) = return x
handleObs (E op k) = case decomp op of
  Right (Observe d y α) -> handleObs (qApp k y)
  Left op' -> E op' (tsingleton $ qComp k handleObs)

handleSamp :: Eff '[Sample] a -> Sampler a
handleSamp  (Val x)  = return x
handleSamp  (E op k) = case decomp op of
  Right (Sample (DistDict d) α) ->
    do  x <- sample d
        handleSamp (qApp k x)
  _        -> error "Impossible: Nothing cannot occur"


simulateMany :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [Env env]                      -- List of model observed variables
  -> Sampler [(a, Env env)]
simulateMany n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  outputs_smaps <- concat <$> mapM runN (zip xs envs)
  let outputs_envs = map (fmap (fromSTrace @env)) outputs_smaps
  return outputs_envs

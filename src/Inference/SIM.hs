{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inference.SIM (simulate, runSimulate, traceSamples, handleSamp, handleObs) where

import Data.Map (Map)
import Effects.Dist ( Observe(..), Sample(..), Dist )
import Effects.ObsReader ( ObsReader )
import Effects.State ( State, modify, handleState )
import Env ( Env )
import Model ( Model, handleCore )
import OpenSum (OpenSum)
import PrimDist
import Prog ( Member(prj), Prog(..), discharge )
import qualified Data.Map as Map
import qualified OpenSum
import Sampler ( Sampler )
import Trace ( FromSTrace(..), STrace, updateSTrace )
import Unsafe.Coerce (unsafeCoerce)

-- ** Simulation (Section 6.1) 

-- | Simulation
simulate :: (FromSTrace env, es ~ '[ObsReader env, Dist,State STrace, Observe, Sample])
  => 
  -- | A model awaiting an input
  (b -> Model env es a)  
  -- | A model environment
  -> Env env               
  -- | Model input 
  -> b                    
  -- | Model output and output environment  
  -> Sampler (a, Env env)   
simulate model env x  = do
  outputs_strace <- runSimulate env (model x)
  return (fmap fromSTrace outputs_strace)

-- | Handler for simulating once from a probabilistic program
runSimulate :: (es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
 => Env env -> Model env es a -> Sampler (a, STrace)
runSimulate env
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore env

-- | Trace sampled values for each Sample operation
traceSamples :: (Member (State STrace) es, Member Sample es) => Prog es a -> Prog es a
traceSamples (Val x) = return x
traceSamples (Op op k) = case prj op of 
  Just (Sample (PrimDistDict d) α) ->
       Op op (\x -> do modify (updateSTrace α d x);
                       traceSamples (k x))
  Nothing -> Op op (traceSamples . k)

-- | Handler @Observe@ operations by simply passing forward their observed value, performing no side-effects
handleObs :: Prog (Observe : es) a -> Prog es  a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
handleSamp :: Prog '[Sample] a -> Sampler a
handleSamp  (Val x)  = return x
handleSamp  (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
    do  x <- sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"

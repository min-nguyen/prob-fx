{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | Simulation.
-}

module Inference.SIM (
    simulate
  , runSimulate
  , traceSamples
  , handleSamp
  , handleObs) where

import Data.Map (Map)
import Effects.Dist ( Observe(..), Sample(..), Dist )
import Effects.ObsReader ( ObsReader )
import Effects.State ( State, modify, handleState )
import Env ( Env )
import Model ( Model, handleCore )
import OpenSum (OpenSum)
import PrimDist ( pattern PrimDistPrf, sample )
import Prog ( Member(prj), Prog(..), discharge )
import qualified Data.Map as Map
import qualified OpenSum
import Sampler ( Sampler )
import Trace ( FromSTrace(..), STrace, updateSTrace )
import Unsafe.Coerce (unsafeCoerce)

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env, es ~ '[ObsReader env, Dist,State STrace, Observe, Sample])
  => Model env es a       -- ^ model
  -> Env env              -- ^ model environment
  -> Sampler (a, Env env) -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

-- | Handler for simulating once from a probabilistic program
runSimulate :: (es ~ '[ObsReader env, Dist, State STrace, Observe, Sample])
 => Model env es a      -- ^ model
 -> Env env             -- ^ model environment
 -> Sampler (a, STrace) -- ^ (model output, sample trace)
runSimulate model
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore model

-- | Trace sampled values for each @Sample@ operation
traceSamples :: (Member (State STrace) es, Member Sample es) => Prog es a -> Prog es a
traceSamples (Val x) = return x
traceSamples (Op op k) = case prj op of
  Just (Sample (PrimDistPrf d) α) ->
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
  Right (Sample (PrimDistPrf d) α) ->
    do  x <- sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"

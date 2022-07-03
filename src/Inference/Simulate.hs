{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Inference.Simulate where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Map (Map)
import Data.Maybe
import Effects.Dist
import Effects.ObsReader
import Effects.State
import Env
import GHC.TypeLits
import Model
import OpenSum (OpenSum)
import PrimDist
import Prog
import qualified Data.Map as Map
import qualified OpenSum as OpenSum
import Sampler
import Trace
import Unsafe.Coerce (unsafeCoerce)

-- ||| (Section 6.1) Simulation
simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist,State STrace, Observe, Sample])
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

-- | Simulate handler
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

handleObs :: Prog (Observe : es) a -> Prog es  a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

handleSamp :: Prog '[Sample] a -> Sampler a
handleSamp  (Val x)  = return x
handleSamp  (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
    do  x <- sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"

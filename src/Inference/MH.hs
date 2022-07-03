{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Inference.MH where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe
-- import Data.Extensible hiding (Member)
import Env
import Control.Monad
import Control.Monad.Trans.Class
import Effects.Dist
import Prog
import Model hiding (runModelFree)
import Sampler
import PrimDist
import Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Effects.ObsReader
import Effects.State
import Unsafe.Coerce
import Inference.Simulate (handleObs, traceSamples)

-- ||| (Section 6.2.2) Metropolis-Hastings
mh :: (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  => 
  -- | Number of MH iterations
     Int                    
  -- | A model awaiting an input
  -> (b -> Model env es a)  
  -- | A model input and model environment (containing observed values to condition on)
  -> (b, Env env)           
  -- | An optional list of observable variable names (strings) to specify sample sites of interest (e.g. for interest in sampling #mu, provide "mu"). This causes other variables to not be resampled unless necessary.
  -> [Tag]                  
  -- | Trace of output environments, containing values sampled for each MH iteration
  -> Sampler [Env env]     
mh n model  (x_0, env_0) tags = do
  -- Perform initial run of MH with no proposal sample site
  y0 <- runMH env_0 Map.empty ("", 0) (model x_0)
  -- Perform n MH iterations
  mhTrace <- foldl (>=>) return (replicate n (mhStep env_0 (model x_0) tags)) [y0]
  -- Return sample trace
  return $ map (\((_, strace), _) -> fromSTrace strace) mhTrace

-- | Perform one step of MH
mhStep :: (es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  => 
  -- | Model environment
     Env env                  
  -- | Model
  -> Model env es a            
  -- | Tags indicating sample sites of interest
  -> [Tag]                     
  -- | Trace of previous MH outputs
  -> [((a, STrace), LPTrace)]  
  -- | Updated trace of MH outputs
  -> Sampler [((a, STrace), LPTrace)]
mhStep env model tags trace = do
  let -- Get previous mh output
      ((x, samples), logps) = head trace

      sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples

  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites

  ((x', samples'), logps') <- runMH env samples α_samp model

  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1)

  if u < acceptance_ratio
    then do return (((x', samples'), logps'):trace)
    else do return trace

-- | MH handler
runMH :: (es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  => 
  -- | Model environment
     Env env       
  -- | Sample trace of previous MH iteration
  -> STrace         
  -- | Sample address of interest
  -> Addr           
  -- | Model
  -> Model env es a 
  -> Sampler ((a, STrace), LPTrace)
runMH env strace α_samp =
     handleSamp strace α_samp  . handleObs
   . handleState Map.empty . handleState Map.empty
   . traceLPs . traceSamples . handleCore env

pattern Samp :: Member Sample es => PrimDist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj  -> Just (Sample d α))

pattern Obs :: Member Observe es => PrimDist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

-- | Trace log probabilities for each Sample and Observe operation
traceLPs :: (Member (State LPTrace) es, Member Sample es, Member Observe es) => Prog es a -> Prog es a
traceLPs (Val x) = return x
traceLPs (Op op k) = case op of
  Samp (PrimDistDict d) α ->
       Op op (\x -> modify (updateLPTrace α d x) >>
                    traceLPs (k x))
  Obs d y α ->
    Op op (\ x -> modify (updateLPTrace α d y) >>
                  traceLPs (k y))
  _         -> Op op (traceLPs . k)

-- | Selectively sample
handleSamp :: STrace -> Addr -> Prog '[Sample] a -> Sampler a
handleSamp strace α_samp (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
        do x <- lookupSample strace d α α_samp
           handleSamp strace α_samp (k x)
  _  -> error "Impossible: Nothing cannot occur"
handleSamp _ _ (Val x) = return x

-- | Look up the sampled value for an address from the sample trace
lookupSample :: Show a => OpenSum.Member a PrimVal => STrace -> PrimDist a -> Addr -> Addr -> Sampler a
lookupSample samples d α α_samp
  | α == α_samp = sample d
  | otherwise   =
      case Map.lookup α samples of
        Just (ErasedPrimDist d', x) -> do
          if d == unsafeCoerce d'
            then return (fromJust $ OpenSum.prj x)
            else sample d
        Nothing -> sample d

-- | Compute acceptance probability
accept :: Addr -> STrace -> STrace -> LPTrace -> LPTrace -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  let dom_logα   = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  let _Xlogα     = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ))
                         0 (Map.keysSet logℙ \\ _Xsampled)
  let _X'logα    = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         0 (Map.keysSet logℙ' \\ _X'sampled)
  return $ exp (dom_logα + _X'logα - _Xlogα)

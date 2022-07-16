{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | Metropolis-Hastings inference
-}

module Inference.MH (
    mh
  , mhStep
  , runMH
  , traceLPs
  , handleSamp
  , lookupSample
  , accept) where

import Control.Monad ( (>=>) )
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Set (Set, (\\))
import Effects.Dist ( Addr, Tag, Observe(..), Sample(..), Dist )
import Effects.ObsReader ( ObsReader )
import Effects.State ( State, modify, handleState )
import Env ( Env )
import Inference.SIM (handleObs, traceSamples)
import Model ( Model, handleCore )
import OpenSum (OpenSum(..))
import PrimDist
    ( ErasedPrimDist(ErasedPrimDist),
      PrimVal,
      PrimDist(UniformDist, DiscrUniformDist),
      pattern PrimDistPrf,
      sample )
import Prog ( Member(prj), EffectSum, Prog(..), discharge )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified OpenSum
import Sampler ( Sampler, liftS )
import Trace ( LPTrace, FromSTrace(..), STrace, updateLPTrace )
import Unsafe.Coerce ( unsafeCoerce )

-- | Top-level wrapper for Metropolis-Hastings (MH) inference
mh :: (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  -- | Number of MH iterations
  => Int
  -- | Model awaiting an input
  -> (b -> Model env es a)
  -- | (Model input, input model environment)
  -> (b, Env env)
  -- | An optional list of observable variable names (strings) to specify sample sites of interest.
  {- For example, provide "mu" to specify interest in sampling #mu. This causes other variables to not be resampled unless necessary. -}
  -> [Tag]
  -- | [Output model environment]
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
  -- | Model environment
  => Env env
  -- | Model
  -> Model env es a
  -- | Tags indicating sample sites of interest
  -> [Tag]
  -- | Trace of previous MH outputs
  -> [((a, STrace), LPTrace)]
  -- | Updated trace of MH outputs
  -> Sampler [((a, STrace), LPTrace)]
mhStep env model tags trace = do
  -- Get previous mh output
  let ((x, samples), logps) = head trace
  -- Get possible addresses to propose new samples for
      sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  -- Draw a proposal sample address
  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address
  ((x', samples'), logps') <- runMH env samples α_samp model
  -- Compute acceptance ratio
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1)
  if u < acceptance_ratio
    then do return (((x', samples'), logps'):trace)
    else do return trace

-- | Handler for one iteration of MH
runMH :: (es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  -- | Model environment
  => Env env
  -- | Sample trace of previous MH iteration
  -> STrace
  -- | Sample address of interest
  -> Addr
  -- | Model
  -> Model env es a
  -- | Sampler generating: (model output, sample trace, log-probability trace)
  -> Sampler ((a, STrace), LPTrace)
runMH env strace α_samp =
     handleSamp strace α_samp  . handleObs
   . handleState Map.empty . handleState Map.empty
   . traceLPs . traceSamples . handleCore env

pattern Samp :: Member Sample es => PrimDist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj  -> Just (Sample d α))

pattern Obs :: Member Observe es => PrimDist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

-- | Handler for tracing log-probabilities for each @Sample@ and @Observe@ operation
traceLPs :: (Member (State LPTrace) es, Member Sample es, Member Observe es)
  => Prog es a
  -> Prog es a
traceLPs (Val x) = return x
traceLPs (Op op k) = case op of
  Samp (PrimDistPrf d) α ->
       Op op (\x -> modify (updateLPTrace α d x) >>
                    traceLPs (k x))
  Obs d y α ->
    Op op (\ x -> modify (updateLPTrace α d y) >>
                  traceLPs (k y))
  _         -> Op op (traceLPs . k)

-- | Handler for @Sample@ that selectively reuses old samples or draws new ones
handleSamp ::
  -- | Sample trace
     STrace
  -- | Address of the proposal sample site for the current MH iteration
  -> Addr
  -- | Probabilistic program with just @Sample@ left
  -> Prog '[Sample] a
  -> Sampler a
handleSamp strace α_samp (Op op k) = case discharge op of
  Right (Sample (PrimDistPrf d) α) ->
        do x <- lookupSample strace d α α_samp
           handleSamp strace α_samp (k x)
  _  -> error "Impossible: Nothing cannot occur"
handleSamp _ _ (Val x) = return x

-- | For a given address, look up a sampled value from a sample trace, returning
--   it only if the primitive distribution it was sampled from matches the current one.
lookupSample :: OpenSum.Member a PrimVal
  =>
  -- | Sample trace
     STrace
  -- | Distribution to sample from
  -> PrimDist a
  -- | Address of current sample site
  -> Addr
  -- | Address of proposal sample site
  -> Addr
  -> Sampler a
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
accept ::
  -- | Address of new sampled value
     Addr
  -- | Previous MH sample trace
  -> STrace
  -- | New MH sample trace
  -> STrace
  -- | Previous MH log-probability trace
  -> LPTrace
  -- | Current MH log-probability trace
  -> LPTrace
  -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  let dom_logα   = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  let _Xlogα     = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ))
                         0 (Map.keysSet logℙ \\ _Xsampled)
  let _X'logα    = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         0 (Map.keysSet logℙ' \\ _X'sampled)
  return $ exp (dom_logα + _X'logα - _Xlogα)

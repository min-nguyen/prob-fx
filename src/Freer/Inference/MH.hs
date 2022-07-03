{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Freer.Inference.MH where

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
import Freer.Effects.Dist

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Freer.Model hiding (runModelFree)
import Sampler
import Freer.Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Freer.Effects.ObsReader
import Freer.Effects.State
import Unsafe.Coerce
import Freer.Inference.Simulate (handleObs, traceSamples)

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

-- | Run MH for multiple data points
mh :: (FromSTrace env, es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
   => Int                              -- Number of mhSteps per data point
   -> (b -> Model env es a)            -- Model awaiting input variable
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> b                              -- List of model input variables
   -> Env env                        -- List of model observed variables
   -> Sampler [Env env]                -- Trace of all accepted outputs, samples, and logps
mh n model tags x_0 env_0 = do
  -- Perform initial run of mh with no proposal sample site
  y0 <- runMH env_0 Map.empty ("", 0) (model x_0)
  -- Perform n mhsteps
  mhTrace <- foldl (>=>) return (replicate n (mhStep env_0 (model x_0) tags)) [y0]
  -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
  return $ map (\((_, strace), _) -> fromSTrace strace) mhTrace

-- | Perform one step of MH for a single data point
mhStep :: (es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  => Env env                  -- Model observed variable
  -> Model env es a           -- Model
  -> [Tag]                    -- Tags indicating sample sites of interest
  -> [((a, STrace), LPTrace)]     -- Trace of previous mh outputs
  -> Sampler [((a, STrace), LPTrace)]
mhStep env model tags trace = do
  let -- Get previous mh output
      ((x, samples), logps) = head trace

      sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples

  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites

  ((x', samples'), logps') <- runMH env samples α_samp model

  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1 Nothing Nothing)

  if u < acceptance_ratio
    then do return (((x', samples'), logps'):trace)
    else do return trace

-- | Run model once under MH
runMH :: (es ~ '[ObsReader env, Dist, State STrace, State LPTrace, Observe, Sample])
  => Env env        -- Model observed variable
  -> STrace         -- Previous mh sample set
  -> Addr           -- Sample address
  -> Model env es a -- Model
  -> Sampler ((a, STrace), LPTrace)
runMH env strace α_samp =
     handleSamp strace α_samp  . handleObs
   . handleState Map.empty . handleState Map.empty
   . traceLPs . traceSamples . handleCore env

pattern Samp :: Member Sample es => Dist x -> Addr -> Union es x
pattern Samp d α <- (prj  -> Just (Sample d α))

pattern Obs :: Member Observe es => Dist x -> x -> Addr -> Union es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

traceLPs :: (Member (State LPTrace) es, Member Sample es, Member Observe es) => Eff es a -> Eff es a
traceLPs (Val x) = return x
traceLPs (E op k) = case op of
  Samp (DistDict d) α ->
    E op (tsingleton $ \x -> modify (updateLPTrace α d x) >> traceLPs (qApp k x))
  Obs d y α ->
    E op (tsingleton $ \x -> modify (updateLPTrace α d y) >> traceLPs (qApp k x))
    -- traceLPs (qApp k y)
  _         -> E op (tsingleton $ qComp k traceLPs)

handleSamp :: STrace -> Addr -> Eff '[Sample] a -> Sampler a
handleSamp strace α_samp (E op k) = case decomp op of
  Right (Sample (DistDict d) α) ->
        do x <- lookupSample strace d α α_samp
           handleSamp strace α_samp (qApp k x)
  _  -> error "Impossible: Nothing cannot occur"
handleSamp _ _ (Val x) = return x

lookupSample :: Show a => OpenSum.Member a PrimVal => STrace -> Dist a -> Addr -> Addr -> Sampler a
lookupSample samples d α α_samp
  | α == α_samp = sample d
  | otherwise   =
      case Map.lookup α samples of
        Just (PrimDist d', x) -> do
          if d == unsafeCoerce d'
            then return (fromJust $ OpenSum.prj x)
            else sample d
        Nothing -> sample d

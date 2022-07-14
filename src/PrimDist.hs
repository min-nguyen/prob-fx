{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PrimDist where

import Data.Kind
import Data.Map (Map)
import Numeric.Log
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import qualified System.Random.MWC.Distributions as MWC
import Sampler
import Statistics.Distribution
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.Dirichlet
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Gamma
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson
import Statistics.Distribution.Uniform
import Util ( boolToInt )

-- ** (Section 4.2.1) Primitive distributions 

-- | Primitive distributions
data PrimDist a where
  HalfCauchyDist    :: Double -> PrimDist Double
  CauchyDist        :: Double -> Double -> PrimDist Double
  NormalDist        :: Double -> Double -> PrimDist Double
  HalfNormalDist    :: Double -> PrimDist Double
  UniformDist       :: Double -> Double -> PrimDist Double
  DiscrUniformDist  :: Int    -> Int    -> PrimDist Int
  GammaDist         :: Double -> Double -> PrimDist Double
  BetaDist          :: Double -> Double -> PrimDist Double
  BinomialDist      :: Int    -> Double -> PrimDist Int
  BernoulliDist     :: Double -> PrimDist Bool
  CategoricalDist   :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> PrimDist a
  DiscreteDist      :: [Double] -> PrimDist Int
  PoissonDist       :: Double -> PrimDist Int
  DirichletDist     :: [Double] -> PrimDist [Double]
  DeterministicDist :: (Eq a, Show a, OpenSum.Member a PrimVal) => a -> PrimDist a

instance Eq (PrimDist a) where
  (==) (NormalDist m s) (NormalDist m' s') = m == m' && s == s'
  (==) (CauchyDist m s) (CauchyDist m' s') = m == m' && s == s'
  (==) (HalfCauchyDist s) (HalfCauchyDist s') = s == s'
  (==) (HalfNormalDist s) (HalfNormalDist s') = s == s'
  (==) (BernoulliDist p) (BernoulliDist p') = p == p'
  (==) (BinomialDist n p) (BinomialDist n' p') = n == n' && p == p'
  (==) (DiscreteDist ps) (DiscreteDist ps') = ps == ps'
  (==) (BetaDist a b) (BetaDist a' b') = a == a' && b == b'
  (==) (GammaDist a b) (GammaDist a' b') = a == a' && b == b'
  (==) (UniformDist a b) (UniformDist a' b') = a == a' && b == b'
  (==) (DiscrUniformDist min max) (DiscrUniformDist min' max') = min == min' && max == max'
  (==) (PoissonDist l) (PoissonDist l') = l == l'
  (==) (CategoricalDist xs) (CategoricalDist xs') = xs == xs'
  (==) (DirichletDist xs) (DirichletDist xs')  = xs == xs'
  (==) (DeterministicDist x) (DeterministicDist x') = x == x'
  (==) _ _ = False

instance Show a => Show (PrimDist a) where
  show (CauchyDist mu sigma) =
   "CauchyDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfCauchyDist sigma) =
   "HalfCauchyDist(" ++ show sigma ++ ", " ++ ")"
  show (NormalDist mu sigma) =
   "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfNormalDist sigma) =
   "HalfNormalDist(" ++ show sigma ++ ", " ++ ")"
  show (BernoulliDist p) =
   "BernoulliDist(" ++ show p ++ ", " ++ ")"
  show (BinomialDist n p) =
   "BinomialDist(" ++ show n ++ ", " ++ show p ++ ", " ++  ")"
  show (DiscreteDist ps) =
   "DiscreteDist(" ++ show ps ++ ", " ++ ")"
  show (BetaDist a b) =
   "BetaDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (GammaDist a b) =
   "GammaDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (UniformDist a b) =
   "UniformDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (DiscrUniformDist min max) =
   "DiscrUniformDist(" ++ show min ++ ", " ++ show max ++ ", " ++ ")"
  show (PoissonDist l) =
   "PoissonDist(" ++ show l ++ ", " ++ ")"
  show (CategoricalDist xs) =
   "CategoricalDist(" ++ show xs ++ ", " ++ ")"
  show (DirichletDist xs) =
   "DirichletDist(" ++ show xs ++ ", " ++ ")"
  show (DeterministicDist x) =
   "DeterministicDist(" ++ show x ++ ", " ++ ")"
  
-- | For constraining the output types of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

data Dict (a :: Constraint) where
  Dict :: a => Dict a

primDistDict :: PrimDist x -> Dict (Show x, OpenSum.Member x PrimVal)
primDistDict d = case d of
  HalfCauchyDist {} -> Dict
  CauchyDist {} -> Dict
  NormalDist {} -> Dict
  HalfNormalDist  {} -> Dict
  UniformDist  {} -> Dict
  DiscrUniformDist {} -> Dict
  GammaDist {} -> Dict
  BetaDist {} -> Dict
  BinomialDist {} -> Dict
  BernoulliDist {} -> Dict
  CategoricalDist {} -> Dict
  DiscreteDist {} -> Dict
  PoissonDist {} -> Dict
  DirichletDist {} -> Dict
  DeterministicDist {} -> Dict

pattern PrimDistDict :: () => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> PrimDist x
pattern PrimDistDict d <- d@(primDistDict -> Dict)

-- | For erasing the types of primitive distributions
data ErasedPrimDist where
  ErasedPrimDist :: forall a. Show a => PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d

-- *** Sampling functions (Section 6.1) 
sample :: PrimDist a -> Sampler a
sample (HalfCauchyDist σ )  =
  createSampler (sampleCauchy 0 σ) >>= pure . abs
sample (CauchyDist μ σ )  =
  createSampler (sampleCauchy μ σ)
sample (HalfNormalDist σ )  =
  createSampler (sampleNormal 0 σ) >>= pure . abs
sample (NormalDist μ σ )  =
  createSampler (sampleNormal μ σ)
sample (UniformDist min max )  =
  createSampler (sampleUniform min max)
sample (DiscrUniformDist min max )  =
  createSampler (sampleDiscreteUniform min max)
sample (GammaDist k θ )        =
  createSampler (sampleGamma k θ)
sample (BetaDist α β  )         =
  createSampler (sampleBeta α β)
sample (BinomialDist n p  )     =
  createSampler (sampleBinomial n p) >>=  pure .  length . filter (== True)
sample (BernoulliDist p )      =
  createSampler (sampleBernoulli p)
sample (CategoricalDist ps )   =
  createSampler (sampleCategorical (V.fromList $ fmap snd ps)) >>= \i -> pure $ fst $ ps !! i
sample (DiscreteDist ps )      =
  createSampler (sampleDiscrete ps)
sample (PoissonDist λ ) =
  createSampler (samplePoisson λ)
sample (DirichletDist xs ) =
  createSampler (sampleDirichlet xs)
sample (DeterministicDist x) = pure x

-- *** Probability density functions (Section 6.2) 
prob :: PrimDist a -> a -> Double
prob (DirichletDist xs) ys =
  let xs' = map (/(Prelude.sum xs)) xs
  in  if Prelude.sum xs' /= 1 then error "dirichlet can't normalize" else
      case dirichletDistribution (UV.fromList xs')
      of Left e -> error "dirichlet error"
         Right d -> let Exp p = dirichletDensity d (UV.fromList ys)
                        in  exp p
prob (HalfCauchyDist σ) y
  = if y < 0 then 0 else
            2 * density (cauchyDistribution 0 σ) y
prob (CauchyDist μ σ) y
  = density (cauchyDistribution μ σ) y
prob (HalfNormalDist σ) y
  = if y < 0 then 0 else
            2 * density (normalDistr 0 σ) y
prob (NormalDist μ σ) y
  = density (normalDistr μ σ) y
prob (UniformDist min max) y
  = density (uniformDistr min max) y
prob (GammaDist k θ) y
  = density (gammaDistr k θ) y
prob  (BetaDist α β) y
  = density (betaDistr α β) y
prob (DiscrUniformDist min max) y
  = probability (discreteUniformAB min max) y
prob (BinomialDist n p) y
  = probability (binomial n p) y
prob (BernoulliDist p) i
  = probability (binomial 1 p) (boolToInt i)
prob d@(CategoricalDist ps) y
  = case lookup y ps of
      Nothing -> error $ "Couldn't find " ++ show y ++ " in categorical dist"
      Just p  -> p
prob (DiscreteDist ps) y
  = ps !! y
prob (PoissonDist λ) y
  = probability (poisson λ) y
prob (DeterministicDist x) y
  = 1

logProb :: PrimDist a -> a -> Double
logProb d = log . prob d

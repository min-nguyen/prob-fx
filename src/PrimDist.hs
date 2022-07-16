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

{- | A GADT encoding of (a selection of) primitive distributions 
    along with their corresponding sampling and density functions.
-}

module PrimDist (
  -- * Primitive distribution
    PrimDist(..)
  , PrimVal
  , IsPrimVal(..)
  , pattern PrimDistPrf
  , ErasedPrimDist(..)
  -- * Sampling
  , sample
  -- * Density
  , prob
  , logProb) where

import Data.Kind ( Constraint )
import Data.Map (Map)
import Numeric.Log ( Log(Exp) )
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import qualified System.Random.MWC.Distributions as MWC
import Statistics.Distribution ( ContDistr(density), DiscreteDistr(probability) )
import Statistics.Distribution.Beta ( betaDistr )
import Statistics.Distribution.Binomial ( binomial )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import Statistics.Distribution.Dirichlet ( dirichletDensity, dirichletDistribution )
import Statistics.Distribution.DiscreteUniform ( discreteUniformAB )
import Statistics.Distribution.Gamma ( gammaDistr )
import Statistics.Distribution.Normal ( normalDistr )
import Statistics.Distribution.Poisson ( poisson )
import Statistics.Distribution.Uniform ( uniformDistr )
import Sampler
import Util ( boolToInt )

-- | Primitive distribution
data PrimDist a where
  BernoulliDist     
    :: Double           -- ^ Probability of @True@
    -> PrimDist Bool  
  BetaDist          
    :: Double           -- ^ Shape α
    -> Double           -- ^ Shape β
    -> PrimDist Double
  BinomialDist      
    :: Int              -- ^ Number of trials
    -> Double           -- ^ Probability of successful trial
    -> PrimDist Int     
  CategoricalDist   
    :: (Eq a, Show a, OpenSum.Member a PrimVal) 
    => [(a, Double)]    -- ^ Values and associated probabilities
    -> PrimDist a       
  CauchyDist        
    :: Double           -- ^ Location
    -> Double           -- ^ Scale
    -> PrimDist Double
  HalfCauchyDist      
    :: Double           -- ^ Scale
    -> PrimDist Double
  DeterministicDist 
    :: (Eq a, Show a, OpenSum.Member a PrimVal) 
    => a                -- ^ Value of probability @1@
    -> PrimDist a
  DirichletDist     
    :: [Double]         -- ^ Concentrations
    -> PrimDist [Double]
  DiscreteDist      
    :: [Double]         -- ^ List of @n@ probabilities
    -> PrimDist Int     -- ^ An index from @0@ to @n - 1@
  DiscrUniformDist      
    :: Int              -- ^ Lower-bound @a@
    -> Int              -- ^ Upper-bound @b@
    -> PrimDist Int     
  GammaDist         
    :: Double           -- ^ Shape k
    -> Double           -- ^ Scale θ
    -> PrimDist Double
  NormalDist      
    :: Double           -- ^ Mean
    -> Double           -- ^ Standard deviation
    -> PrimDist Double
  HalfNormalDist    
    :: Double           -- ^ Standard deviation
    -> PrimDist Double
  PoissonDist       
    :: Double           -- ^ Rate λ
    -> PrimDist Int
  UniformDist       
    :: Double           -- ^ Lower-bound @a@
    -> Double           -- ^ Upper-bound @b@
    -> PrimDist Double  

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
  
-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

-- | Proof that @x@ is a primitive value
data IsPrimVal x where
  IsPrimVal :: (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value 
pattern PrimDistPrf :: () => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> PrimDist x
pattern PrimDistPrf d <- d@(primDistPrf -> IsPrimVal)

-- | Proof that all primitive distributions generate a primitive value
primDistPrf :: PrimDist x -> IsPrimVal x 
primDistPrf d = case d of
  HalfCauchyDist {} -> IsPrimVal
  CauchyDist {} -> IsPrimVal
  NormalDist {} -> IsPrimVal
  HalfNormalDist  {} -> IsPrimVal
  UniformDist  {} -> IsPrimVal
  DiscrUniformDist {} -> IsPrimVal
  GammaDist {} -> IsPrimVal
  BetaDist {} -> IsPrimVal
  BinomialDist {} -> IsPrimVal
  BernoulliDist {} -> IsPrimVal
  CategoricalDist {} -> IsPrimVal
  DiscreteDist {} -> IsPrimVal
  PoissonDist {} -> IsPrimVal
  DirichletDist {} -> IsPrimVal
  DeterministicDist {} -> IsPrimVal

-- | For erasing the types of primitive distributions
data ErasedPrimDist where
  ErasedPrimDist :: forall a. Show a => PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d

-- | Draw a value from a primitive distribution in the @Sampler@ monad
sample :: 
     PrimDist a 
  -> Sampler a
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

-- | Compute the density of a primitive distribution generating an observed value
prob :: 
  -- | Distribution
     PrimDist a 
  -- | Observed value
  -> a 
  -- | Density
  -> Double
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

-- | Compute the log density of a primitive distribution generating an observed value
logProb :: 
  -- | Distribution
     PrimDist a 
  -- | Observed value
  -> a 
  -- | Log density
  -> Double
logProb d = log . prob d

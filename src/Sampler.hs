{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sampler where

import Control.Monad
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Word
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import qualified System.Random.MWC.Probability as MWC.Probability
import Statistics.Distribution ( ContGen(genContVar) )
import Statistics.Distribution.CauchyLorentz
import System.Random.MWC

-- | Sampler type, for running IO computations alongside a random number generator
newtype Sampler a = Sampler {runSampler :: ReaderT MWC.GenIO IO a}
  deriving (Functor, Applicative, Monad)

liftS :: IO a -> Sampler a
liftS f = Sampler $ lift f

-- | Takes a Sampler, provides it a random generator, and runs the sampler in the IO context
sampleIO :: Sampler a -> IO a
sampleIO m = MWC.createSystemRandom >>= (runReaderT . runSampler) m

-- | Takes a Sampler, provides it a fixed generator, and runs the sampler in the IO context
sampleIOFixed :: Sampler a -> IO a
sampleIOFixed m = MWC.create >>= (runReaderT . runSampler) m

sampleIOFixedSeed :: Int -> Sampler a -> IO a
sampleIOFixedSeed n m = initialize (V.singleton (fromIntegral n :: Word32)) >>= (runReaderT . runSampler) m

-- | Takes a distribution which awaits a generator, and returns a Sampler
createSampler :: (MWC.GenIO -> IO a) -> Sampler a
createSampler f = Sampler $ ask >>= lift . f

-- | Given distribution parameters, these functions await a generator and then sample a value from a distribution
sampleRandom :: MWC.GenIO -> IO Double
sampleRandom = \gen -> MWC.uniform gen

sampleCauchy :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleCauchy μ σ = \gen -> genContVar (cauchyDistribution μ σ) gen

sampleNormal :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleNormal μ σ = \gen -> MWC.Dist.normal μ σ gen

sampleUniform :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleUniform min max = \gen -> MWC.uniformR (min, max) gen

sampleDiscreteUniform :: Int -> Int -> (MWC.GenIO -> IO Int)
sampleDiscreteUniform min max = \gen -> MWC.uniformR (min, max) gen

sampleGamma :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleGamma k θ = \gen -> MWC.Dist.gamma k θ gen

sampleBeta :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleBeta α β = \gen -> MWC.Dist.beta α β gen

sampleBernoulli :: Double -> (MWC.GenIO -> IO Bool)
sampleBernoulli p = \gen -> MWC.Dist.bernoulli p gen

sampleBinomial :: Int -> Double -> (MWC.GenIO -> IO [Bool])
sampleBinomial n p = \gen -> replicateM n (MWC.Dist.bernoulli p gen)

sampleCategorical :: V.Vector Double -> (MWC.GenIO -> IO Int)
sampleCategorical ps =  \gen -> MWC.Dist.categorical (ps) gen

sampleDiscrete :: [Double] -> (MWC.GenIO -> IO Int)
sampleDiscrete ps = \gen -> MWC.Dist.categorical (V.fromList ps) gen

samplePoisson :: Double -> (MWC.GenIO -> IO Int)
samplePoisson λ = \gen -> MWC.Probability.sample (MWC.Probability.poisson λ) gen

sampleDirichlet :: [Double] -> (MWC.GenIO -> IO [Double])
sampleDirichlet xs = \gen -> MWC.Dist.dirichlet xs gen
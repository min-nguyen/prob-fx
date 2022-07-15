{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{- An algebraic effect embedding of probabilistic models.
-}

module Model 
  ( -- * Model
    Model(..)
  , handleCore
    -- * Distribution smart constructors
  , bernoulli
  , bernoulli'
  , beta
  , beta'
  , binomial
  , binomial'
  , categorical
  , categorical'
  , cauchy
  , cauchy'
  , halfCauchy
  , halfCauchy'
  , deterministic
  , deterministic'
  , dirichlet
  , dirichlet'
  , discrete
  , discrete'
  , gamma
  , gamma'
  , normal
  , normal'
  , halfNormal
  , halfNormal'
  , poisson
  , poisson'
  , uniform
  , uniform'
  ) 
  where

import Control.Monad ( ap )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Effects.Dist ( handleDist, Dist(Dist), Observe, Sample )
import Effects.Lift ( Lift(..) )
import Effects.ObsReader ( ask, handleRead, ObsReader )
import Effects.State ( State, getSt, putSt, modify, handleState )
import Env ( varToStr, Env, ObsVar, Observable )
import OpenSum (OpenSum)
import PrimDist
import Prog ( call, Member, Prog )
import qualified OpenSum

-- | A @Model@ is indexed by a model environment @env@ containing random variables that can be provided observed values for, an effect signature @es@ of the possible effects a model can invoke, and an output type @a@ of values that the model generates.
newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (ObsReader env) es, Member Sample es) => Prog es a }
  deriving Functor

instance Applicative (Model env es) where
  pure x = Model $ pure x
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

-- | The initial handler for models, specialising a model under a certain environment to produce a probabilistic program consisting of @Sample@ and @Observe@ operations. 
handleCore :: (Member Observe es, Member Sample es) => Env env -> Model env (ObsReader env : Dist : es) a -> Prog es a
handleCore env m = (handleDist . handleRead env) (runModel m)

-- ** Distribution smart constructors
{- $ hello

-}

deterministic :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a)
  => a 
  -> ObsVar x 
  -> Model env es a
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DeterministicDist x) maybe_y tag)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal)
  => a -> Model env es a
deterministic' x = Model $ do
  call (Dist (DeterministicDist x) Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double]
  => [Double] -> ObsVar x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DirichletDist xs) maybe_y tag)

dirichlet' :: [Double] -> Model env es [Double]
dirichlet' xs = Model $ do
  call (Dist (DirichletDist xs) Nothing Nothing)

discrete :: forall env es x. Observable env x Int
  => [Double] -> ObsVar x
  -> Model env es Int
discrete xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DiscreteDist xs) maybe_y tag)

discrete' :: [Double] -> Model env es Int
discrete' xs = Model $ do
  call (Dist (DiscreteDist xs) Nothing Nothing)

categorical :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal) => Observable env x a
  => [(a, Double)] -> ObsVar x
  -> Model env es a
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (CategoricalDist xs) maybe_y tag)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Model env es a
categorical' xs = Model $ do
  call (Dist (CategoricalDist xs) Nothing Nothing)

normal :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (NormalDist mu sigma) maybe_y tag)

normal' :: Double -> Double -> Model env es Double
normal' mu sigma = Model $ do
  call (Dist (NormalDist mu sigma) Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfNormalDist sigma) maybe_y tag)

halfNormal' :: Double -> Model env es Double
halfNormal' sigma = Model $ do
  call (Dist (HalfNormalDist sigma) Nothing Nothing)

cauchy :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (CauchyDist mu sigma) maybe_y tag)

cauchy' :: Double -> Double -> Model env es Double
cauchy' mu sigma = Model $ do
  call (Dist (CauchyDist mu sigma) Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfCauchyDist sigma) maybe_y tag)

halfCauchy' :: Double -> Model env es Double
halfCauchy' sigma = Model $ do
  call (Dist (HalfCauchyDist sigma) Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool
  => Double -> ObsVar x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BernoulliDist p) maybe_y tag)

bernoulli' :: Double -> Model env es Bool
bernoulli' p = Model $ do
  call (Dist (BernoulliDist p) Nothing Nothing)

binomial :: forall env es x. Observable env x Int
  => Int -> Double -> ObsVar x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BinomialDist n p) maybe_y tag)

binomial' :: Int -> Double -> Model env es Int
binomial' n p = Model $ do
  call (Dist (BinomialDist n p) Nothing Nothing)

gamma :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
gamma x θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (GammaDist x θ) maybe_y tag)

gamma' :: Double -> Double -> Model env es Double
gamma' x θ = Model $ do
  call (Dist (GammaDist x θ) Nothing Nothing)

beta :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BetaDist α β) maybe_y tag)

beta' :: Double -> Double -> Model env es Double
beta' α β = Model $ do
  call (Dist (BetaDist α β) Nothing Nothing)

uniform :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (UniformDist min max) maybe_y tag)

uniform' :: Double -> Double -> Model env es Double
uniform' min max = Model $ do
  call (Dist (UniformDist min max) Nothing Nothing)

poisson :: forall env es x. Observable env x Int
  => Double -> ObsVar x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (PoissonDist λ) maybe_y tag)

poisson' :: Double -> Model env es Int
poisson' λ = Model $ do
  call (Dist (PoissonDist λ) Nothing Nothing)


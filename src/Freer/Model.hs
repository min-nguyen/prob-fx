{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Freer.Model where

import Freer.Effects.Dist
-- import Freer.Prog
import Freer.Effects.ObsReader
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Freer.Effects.State ( State, getSt, putSt, modify, handleState )
import Env
import Control.Monad ( ap )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Freer

{- Model -}
newtype Model env es v =
  Model { runModel :: (Member Dist es, Member (ObsReader env) es, Member Sample es) => Eff es v }
  deriving Functor

instance Applicative (Model env es) where
  pure x = Model $ pure x
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

{- Transform multimodal model into program of samples and observes -}
handleCore :: (Member Observe es, Member Sample es) => Env env -> Model env (ObsReader env : Dist : es) a -> Eff es a
handleCore env m = (handleDist . handleRead env) (runModel m)

{- Distribution smart constructors -}

deterministic' :: (Eq v, Show v, OpenSum.Member v PrimVal)
  => v -> Model env es v
deterministic' x = Model $ do
  send (DeterministicDist x Nothing Nothing)

deterministic :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal)
  => Observable env x v
  => v -> ObsVar x -> Model env es v
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DeterministicDist x maybe_y tag)

dirichlet' :: [Double] -> Model env es [Double]
dirichlet' xs = Model $ do
  send (DirichletDist xs Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double]
  => [Double] -> ObsVar x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DirichletDist xs maybe_y tag)

discrete' :: [Double] -> Model env es Int
discrete' xs = Model $ do
  send (DiscreteDist xs Nothing Nothing)

discrete :: forall env es x. Observable env x Int
  => [Double] -> ObsVar x
  -> Model env es Int
discrete xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DiscreteDist xs maybe_y tag)

categorical' :: (Eq v, Show v, OpenSum.Member v PrimVal) => [(v, Double)] -> Model env es v
categorical' xs = Model $ do
  send (CategoricalDist xs Nothing Nothing)

categorical :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal) => Observable env x v
  => [(v, Double)] -> ObsVar x
  -> Model env es v
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (CategoricalDist xs maybe_y tag)

normal' :: Double -> Double -> Model env es Double
normal' mu sigma = Model $ do
  send (NormalDist mu sigma Nothing Nothing)

normal :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (NormalDist mu sigma maybe_y tag)

halfNormal' :: Double -> Model env es Double
halfNormal' sigma = Model $ do
  send (HalfNormalDist sigma Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (HalfNormalDist sigma maybe_y tag)

cauchy' :: Double -> Double -> Model env es Double
cauchy' mu sigma = Model $ do
  send (CauchyDist mu sigma Nothing Nothing)

cauchy :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (CauchyDist mu sigma maybe_y tag)

halfCauchy' :: Double -> Model env es Double
halfCauchy' sigma = Model $ do
  send (HalfCauchyDist sigma Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (HalfCauchyDist sigma maybe_y tag)

bernoulli' :: Double -> Model env es Bool
bernoulli' p = Model $ do
  send (BernoulliDist p Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool
  => Double -> ObsVar x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BernoulliDist p maybe_y tag)

binomial' :: Int -> Double -> Model env es Int
binomial' n p = Model $ do
  send (BinomialDist n p Nothing Nothing)

binomial :: forall env es x. Observable env x Int
  => Int -> Double -> ObsVar x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BinomialDist n p maybe_y tag)

gamma' :: Double -> Double -> Model env es Double
gamma' x θ = Model $ do
  send (GammaDist x θ Nothing Nothing)

gamma :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
gamma x θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (GammaDist x θ maybe_y tag)

beta' :: Double -> Double -> Model env es Double
beta' α β = Model $ do
  send (BetaDist α β Nothing Nothing)

beta :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BetaDist α β maybe_y tag)

uniform' :: Double -> Double -> Model env es Double
uniform' min max = Model $ do
  send (UniformDist min max Nothing Nothing)

uniform :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (UniformDist min max maybe_y tag)

poisson' :: Double -> Model env es Int
poisson' λ = Model $ do
  send (PoissonDist λ Nothing Nothing)

poisson :: forall env es x. Observable env x Int
  => Double -> ObsVar x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (PoissonDist λ maybe_y tag)

{- Extra effect functions -}

putM :: Member (State s) es => s -> Model env es ()
putM x = Model $ putSt x

getM :: Member (State s) es => Model env es s
getM = Model getSt

modifyM :: Member (State s) es => (s -> s) -> Model env es ()
modifyM f = Model $ modify f

runStateM :: Model env (State [Int] : es) v -> Model env es (v, [Int])
runStateM m = Model $ handleState [] $ runModel m

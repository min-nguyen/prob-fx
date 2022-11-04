{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    Model(..)
  , handleCore
    -- * Distribution smart constructors
    -- $Smart-Constructors
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
import Effects.State ( State, modify, handleState )
import Env ( varToStr, Env, ObsVar, Observable )
import PrimDist ( PrimVal, PrimDist(..) )
import Prog ( call, Member, Prog )
import qualified OpenSum

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions
    and @ObsReader env@ for reading from @env@.
-}
newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (ObsReader env) es) => Prog es a }
  deriving Functor

instance Applicative (Model env es) where
  pure x = Model $ pure x
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

{- | The initial handler for models, specialising a model under a certain
environment to produce a probabilistic program consisting of @Sample@ and @Observe@ operations. -}
handleCore :: (Member Observe es, Member Sample es) => Model env (ObsReader env : Dist : es) a -> Env env -> Prog es a
handleCore m env = (handleDist . handleRead env) (runModel m)

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => Model env es Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: Model env es Bool
    exampleModel' = bernoulli' 0.5
    @
-}

deterministic :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) => a
  -> ObsVar x
  -> Model env es a
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DeterministicDist x) maybe_y tag)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal) =>
     a -- ^ value to be deterministically generated
  -> Model env es a
deterministic' x = Model $ do
  call (Dist (DeterministicDist x) Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double] =>
     [Double]
  -> ObsVar x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DirichletDist xs) maybe_y tag)

dirichlet' ::
     [Double] -- ^ concentration parameters
  -> Model env es [Double]
dirichlet' xs = Model $ do
  call (Dist (DirichletDist xs) Nothing Nothing)

discrete :: forall env es x. Observable env x Int =>
     [Double]
  -> ObsVar x
  -> Model env es Int
discrete ps field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (DiscreteDist ps) maybe_y tag)

discrete' ::
     [Double]         -- ^ list of @n@ probabilities
  -> Model env es Int -- ^ integer index from @0@ to @n - 1@
discrete' ps = Model $ do
  call (Dist (DiscreteDist ps) Nothing Nothing)

categorical :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) =>
     [(a, Double)]
  -> ObsVar x
  -> Model env es a
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (CategoricalDist xs) maybe_y tag)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal) =>
     [(a, Double)] -- ^ primitive values and their probabilities
  -> Model env es a
categorical' xs = Model $ do
  call (Dist (CategoricalDist xs) Nothing Nothing)

normal :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (NormalDist mu sigma) maybe_y tag)

normal' ::
     Double -- ^ mean
  -> Double -- ^ standard deviation
  -> Model env es Double
normal' mu sigma = Model $ do
  call (Dist (NormalDist mu sigma) Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double =>
     Double
  -> ObsVar x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfNormalDist sigma) maybe_y tag)

halfNormal' ::
     Double -- ^ standard deviation
  -> Model env es Double
halfNormal' sigma = Model $ do
  call (Dist (HalfNormalDist sigma) Nothing Nothing)

cauchy :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (CauchyDist mu sigma) maybe_y tag)

cauchy' ::
     Double -- ^ location
  -> Double -- ^ scale
  -> Model env es Double
cauchy' mu sigma = Model $ do
  call (Dist (CauchyDist mu sigma) Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double =>
     Double
  -> ObsVar x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfCauchyDist sigma) maybe_y tag)

halfCauchy' ::
     Double -- ^ scale
  -> Model env es Double
halfCauchy' sigma = Model $ do
  call (Dist (HalfCauchyDist sigma) Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool =>
     Double
  -> ObsVar x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BernoulliDist p) maybe_y tag)

bernoulli' ::
     Double -- ^ probability of @True@
  -> Model env es Bool
bernoulli' p = Model $ do
  call (Dist (BernoulliDist p) Nothing Nothing)

beta :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BetaDist α β) maybe_y tag)

beta' ::
     Double -- ^ shape 1 (α)
  -> Double -- ^ shape 2 (β)
  -> Model env es Double
beta' α β = Model $ do
  call (Dist (BetaDist α β) Nothing Nothing)

binomial :: forall env es x. Observable env x Int =>
     Int
  -> Double
  -> ObsVar x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (BinomialDist n p) maybe_y tag)

binomial' ::
     Int              -- ^ number of trials
  -> Double           -- ^ probability of successful trial
  -> Model env es Int -- ^ number of successful trials
binomial' n p = Model $ do
  call (Dist (BinomialDist n p) Nothing Nothing)

gamma :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env es Double
gamma k θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (GammaDist k θ) maybe_y tag)

gamma' ::
     Double -- ^ shape (k)
  -> Double -- ^ scale (θ)
  -> Model env es Double
gamma' k θ = Model $ do
  call (Dist (GammaDist k θ) Nothing Nothing)

uniform :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (UniformDist min max) maybe_y tag)

uniform' ::
     Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Model env es Double
uniform' min max = Model $ do
  call (Dist (UniformDist min max) Nothing Nothing)

poisson :: forall env es x. Observable env x Int =>
     Double
  -> ObsVar x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (PoissonDist λ) maybe_y tag)

poisson' ::
     Double           -- ^ rate (λ)
  -> Model env es Int -- ^ number of events
poisson' λ = Model $ do
  call (Dist (PoissonDist λ) Nothing Nothing)


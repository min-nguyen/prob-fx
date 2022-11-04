{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | This implements the model environments that users must provide upon running a model;
     such environments assign traces of values to the "observable variables" (random
     variables which can be conditioned against) of a model.
-}

module Env
  ( -- * Observable variable
    ObsVar(..)
  , varToStr
    -- * Model environment
  , Assign(..)
  , Env(..)
  , (<:>)
  , nil
  , Observable(..)
  , Observables(..)
  , UniqueKey
  , LookupType) where

import Data.Kind ( Constraint )
import Data.Proxy ( Proxy(Proxy) )
import FindElem ( FindElem(..), Idx(..) )
import GHC.OverloadedLabels ( IsLabel(..) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Unsafe.Coerce ( unsafeCoerce )

-- | Containers for observable variables
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

-- | Allows the syntax @#x@ to be automatically lifted to the type @ObsVar "x"@.
instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

-- | Convert an observable variable from a type-level string to a value-level string
varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

-- * Model Environments

{- | A model environment assigning traces (lists) of observed values to observable
     variables i.e. the type @Env ((x := a) : env)@ indicates @x@ is assigned a value
     of type @[a]@.
-}
data Env (env :: [Assign Symbol *]) where
  ENil  :: Env '[]
  ECons :: [a] -> Env env -> Env (x := a : env)

-- | Assign or associate a variable @x@ with a value of type @a@
data Assign x a = x := a

-- | Empty model environment
nil :: Env '[]
nil = ENil

infixr 5 <:>
-- | Prepend a variable assignment to a model environment
(<:>) :: UniqueKey x env ~ True => Assign (ObsVar x) [a] -> Env env -> Env ((x ':= a) ': env)
(_ := as) <:> env = ECons as env

instance (KnownSymbol x, Show a, Show (Env env)) => Show (Env ((x := a) ': env)) where
  show (ECons a env) = varToStr (ObsVar @x) ++ ":=" ++ show a ++ ", " ++ show env
instance Show (Env '[]) where
  show ENil = "[]"

instance FindElem x ((x := a) : env) where
  findElem = Idx 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x ((x' := a) : env) where
  findElem = Idx $ 1 + unIdx (findElem :: Idx x env)

-- | Retrieve the type of an observable variable @x@ from an environment @env@
type family LookupType x env where
  LookupType x ((x := a) : env) = a
  LookupType x ((x' := a) : env) = LookupType x env

-- | Specifies that an environment @Env env@ has an observable variable @x@ whose observed values are of type @a@
class (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  get  :: ObsVar x -> Env env -> [a]
  set  :: ObsVar x -> [a] -> Env env -> Env env

instance (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  get _ env =
    let idx = unIdx $ findElem @x @env
        f :: Int -> Env env' -> [a]
        f n (ECons a env) 
         | n == 0    = unsafeCoerce a
         | otherwise = f (n - 1) env
    in  f idx env
  set _ a' env =
    let idx = unIdx $ findElem @x @env
        f :: Int -> Env env' -> Env env'
        f n (ECons a env)
          | n == 0    = ECons (unsafeCoerce a') env
          | otherwise = ECons a (f (n - 1) env)
    in  f idx env

-- | For each observable variable @x@ in @xs@, construct the constraint @Observable env x a@
type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

-- | Check whether an observable variable @x@ is unique in model environment @env@
type family UniqueKey x env where
  UniqueKey x ((x ':= a) : env) = False
  UniqueKey x ((x' ':= a) : env) = UniqueKey x env
  UniqueKey x '[] = True

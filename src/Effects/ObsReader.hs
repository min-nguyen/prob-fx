{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | The effect for reading observable variables from a model environment.
-}

module Effects.ObsReader (
    ObsReader(..)
  , ask
  , handleRead) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, ObsVar, Observable(..) )
import Data.Maybe (listToMaybe)

-- | The effect for reading observed values from a model environment @env@
data ObsReader env a where
  -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  Ask :: Observable env x a
    => ObsVar x                 -- ^ variable @x@ to read from
    -> ObsReader env (Maybe a)  -- ^ the head value from @x@'s list

-- | Wrapper function for calling @Ask@
ask :: forall env es x a. (Member (ObsReader env) es, Observable env x a)
  => ObsVar x
  -> Prog es (Maybe a)
ask x = call (Ask @env x)

-- | Handle the @Ask@ requests of observable variables
handleRead ::
  -- | initial model environment
     Env env
  -> Prog (ObsReader env ': es) a
  -> Prog es a
handleRead env (Val x) = return x
handleRead env (Op op k) = case discharge op of
  Right (Ask x) ->
    let vs       = get x env
        maybe_v  = listToMaybe vs
        env'     = set x (drop 1 vs) env
    in  handleRead env' (k maybe_v)
  Left op' -> Op op' (handleRead env . k)

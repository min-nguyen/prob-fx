{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Freer.Effects.ObsReader where

import Env
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Util

data ObsReader env a where
  Ask :: Observable env x a => ObsVar x -> ObsReader env (Maybe a)

ask :: forall env es x a. Member (ObsReader env) es => Observable env x a => ObsVar x -> Eff es (Maybe a)
ask x = send (Ask @env x)

handleRead :: forall env es a.
  Env env -> Eff (ObsReader env ': es) a -> Eff es a
handleRead env (Val x) = return x
handleRead env (E op k) = case decomp op of
  Right (Ask x) ->
    let vs       = get x env
        maybe_v  = safeHead vs
        env'     = set x (safeTail vs) env
    in  handleRead env' (qApp k maybe_v)
  Left op' -> E op' (tsingleton $ qComp k (handleRead env))
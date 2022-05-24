{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effects.ObsReader where

import Prog
import Env
import Util

-- | ObsReader effect and handler, for reading values of observable variables from model environment
data ObsReader env a where
  Ask :: Observable env x a => ObsVar x -> ObsReader env (Maybe a)

ask :: forall env es x a. Member (ObsReader env) es => Observable env x a => ObsVar x -> Prog es (Maybe a)
ask x = call (Ask @env x)

handleRead :: forall env es a.
  Env env -> Prog (ObsReader env ': es) a -> Prog es a
handleRead env (Val x) = return x
handleRead env (Op op k) = case discharge op of
  Right (Ask x) ->
    let vs       = get x env
        maybe_v  = safeHead vs
        env'     = set x (safeTail vs) env
    in  handleRead env' (k maybe_v)
  Left op' -> Op op' (handleRead env . k)

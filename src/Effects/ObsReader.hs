{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- | An effect for reading observable variables from a model environment -}

module Effects.ObsReader (
    ObsReader(..)
  , ask
  , handleRead) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, ObsVar, Observable(..) )
import Util ( safeHead, safeTail )

-- | The effect for reading observed values from a model environment @env@
data ObsReader env a where
  -- | Given variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  Ask :: Observable env x a       
    => ObsVar x                 -- ^ Variable @x@ to read from
    -> ObsReader env (Maybe a)  -- ^ The head value from @x@'s list

-- | Retrieve the first value from the trace of an observable variable if it exists
ask :: forall env es x a. (Member (ObsReader env) es, Observable env x a) =>  ObsVar x -> Prog es (Maybe a)
ask x = call (Ask @env x)

-- | Handle the @Ask@ requests of observable variables
handleRead :: 
  -- | Initial model environment
     Env env 
  -> Prog (ObsReader env ': es) a 
  -> Prog es a
handleRead env (Val x) = return x
handleRead env (Op op k) = case discharge op of
  Right (Ask x) ->
    let vs       = get x env
        maybe_v  = safeHead vs
        env'     = set x (safeTail vs) env
    in  handleRead env' (k maybe_v)
  Left op' -> Op op' (handleRead env . k)

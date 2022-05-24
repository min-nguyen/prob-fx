{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Freer.Effects.State where

import Control.Monad.Freer
import Control.Monad.Freer.Internal

data State s a where
  GetSt :: State s s
  PutSt :: s -> State s ()

getSt :: (Member (State s) es) => Eff es s
getSt = send GetSt

putSt :: (Member (State s) es) => s -> Eff es ()
putSt s = send (PutSt s)

modify :: Member (State s) es => (s -> s) -> Eff es ()
modify f = getSt >>= putSt . f

handleState :: forall s ts a. s -> Eff (State s ': ts) a -> Eff ts (a, s)
handleState s m = loop s m where
  loop :: s -> Eff (State s ': ts) a -> Eff ts (a, s)
  loop s (Val x) = return (x, s)
  loop s (E u k) = case decomp u of
    Right GetSt      -> loop s (qApp k s)
    Right (PutSt s') -> loop s' (qApp k ())
    Left  u'         -> E u' (tsingleton $ qComp k (loop s))
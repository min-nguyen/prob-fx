{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.State where

import Prog

-- ||| (Section 6.1) State effect and handler
data State s a where
  GetSt :: State s s
  PutSt :: s -> State s ()

getSt :: (Member (State s) es) => Prog es s
getSt = Op (inj GetSt) Val

putSt :: (Member (State s) es) => s -> Prog es ()
putSt s = Op (inj $ PutSt s) Val

modify :: Member (State s) es => (s -> s) -> Prog es ()
modify f = getSt >>= putSt . f

handleState :: forall s es a. s -> Prog (State s ': es) a -> Prog es (a, s)
handleState s m = loop s m where
  loop :: s -> Prog (State s ': es) a -> Prog es (a, s)
  loop s (Val x) = return (x, s)
  loop s (Op u k) = case discharge u of
    Right GetSt      -> loop s (k s)
    Right (PutSt s') -> loop s' (k ())
    Left  u'         -> Op u' (loop s . k)

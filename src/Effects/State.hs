{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | The state effect
-}

module Effects.State (
    State(..)
  , get 
  , put
  , modify
  , handleState) where

import Prog ( discharge, Member(inj), Prog(..) )

-- | The state effect
data State s a where
  Get :: State s s
  Put :: s -> State s ()

-- | Get the state
get :: (Member (State s) es) => Prog es s
get = Op (inj Get) Val

-- | Set the state
put :: (Member (State s) es) => s -> Prog es ()
put s = Op (inj $ Put s) Val

-- | Apply a function to the state
modify :: Member (State s) es => (s -> s) -> Prog es ()
modify f = get >>= put . f

-- | Handle the @State s@ effect
handleState :: 
  -- | Initial state
     s 
  -- | Initial program
  -> Prog (State s ': es) a 
  -- | Pure value and final state
  -> Prog es (a, s)
handleState s m = loop s m where
  loop :: s -> Prog (State s ': es) a -> Prog es (a, s)
  loop s (Val x) = return (x, s)
  loop s (Op u k) = case discharge u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'         -> Op u' (loop s . k)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | Writer effect
-}

module Effects.Writer (
    Writer(..)
  , tell
  , tellM
  , handleWriter
  , handleWriterM) where

import Prog ( discharge, Member(inj), Prog(..) )
import Model ( Model(..) )

-- | Writer effect for writing to a strean @w@
data Writer w a where
  -- | Write to a stream @w@
  Tell :: 
       w  -- ^ Value to write
    -> Writer w ()

-- | Write to a stream @w@
tell :: Member (Writer w) ts => w -> Prog ts ()
tell w = Op (inj $ Tell w) Val
    
-- | Write to a stream @w@ inside a @Model@
tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

-- | Handle the @Writer@ effect
handleWriter :: forall w ts a . Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
handleWriter = loop mempty where
  loop ::  w -> Prog (Writer w ': ts) a -> Prog ts (a, w)
  loop w (Val x) = return (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

-- | Handle the @Writer@ effect inside a @Model@
handleWriterM :: Monoid w => Model env (Writer w : es) v -> Model env es (v, w)
handleWriterM m = Model $ handleWriter $ runModel m

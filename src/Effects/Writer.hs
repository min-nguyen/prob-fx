{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | Writer effect.
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
  Tell :: w             -- ^ value to write
       -> Writer w ()

-- | Wrapper for @Tell@
tell :: Member (Writer w) es => w -> Prog es ()
tell w = Op (inj $ Tell w) Val

-- | Wrapper for @Tell@ inside @Model@
tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

-- | Handle the @Writer@ effect for a stream @w@
handleWriter :: forall w es a. Monoid w
  => Prog (Writer w ': es) a
  -> Prog es (a, w) -- ^ (output, final stream)
handleWriter = loop mempty where
  loop ::  w -> Prog (Writer w ': es) a -> Prog es (a, w)
  loop w (Val x) = return (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

-- | Handle the @Writer@ effect inside a @Model@
handleWriterM :: Monoid w
  => Model env (Writer w : es) a
  -> Model env es (a, w) -- ^ (output, final stream)
handleWriterM m = Model $ handleWriter $ runModel m

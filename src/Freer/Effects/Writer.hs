{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Freer.Effects.Writer where
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Freer.Model

data Writer w a where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) ts => w -> Eff ts ()
tell w = send (Tell w)

handleWriter :: forall w ts a . Monoid w => Eff (Writer w ': ts) a -> Eff ts (a, w)
handleWriter = loop mempty where
  loop ::  w -> Eff (Writer w ': ts) a -> Eff ts (a, w)
  -- At this point, all Reader requests have been handled
  loop w (Val x) = return (x, w)
  -- Handle if Writer request, else ignore and go through the rest of the tree
  loop w (E u q) = case decomp u of
    Right (Tell w') -> loop (w <> w') (qApp q ())
    Left u'         -> E u' (tsingleton $ qComp q (loop w))

tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

handleWriterM :: Monoid w => Model env (Writer w : es) v -> Model env es (v, w)
handleWriterM m = Model $ handleWriter $ runModel m

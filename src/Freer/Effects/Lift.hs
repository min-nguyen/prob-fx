{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Freer.Effects.Lift where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Data.Function (fix)

newtype Lift m a = Lift (m a)

lift :: (Member (Lift m) es) => m a -> Eff es a
lift = send . Lift

runLift :: forall m w. Monad m => Eff '[Lift m] w -> m w
runLift (Val x) = return x
runLift (E u q) = case prj u of
     Just (Lift m) -> m >>= runLift . qApp q
     Nothing -> error "Impossible: Nothing cannot occur"
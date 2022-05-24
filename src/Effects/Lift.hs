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

module Effects.Lift where

import Prog
import Data.Function (fix)

-- | Lift effect for lifting an arbitrary monad `m` into the effect `Lift m`
newtype Lift m a = Lift (m a)

lift :: (Member (Lift m) es) => m a -> Prog es a
lift = call . Lift

runLift :: forall m w. Monad m => Prog '[Lift m] w -> m w
runLift (Val x) = return x
runLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= runLift . q
     Nothing -> error "Impossible: Nothing cannot occur"
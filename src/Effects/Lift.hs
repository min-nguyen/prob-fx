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

{- | For lifting arbitrary monadic computations into an algebraic effect setting
-}

module Effects.Lift (Lift(..), lift, handleLift) where

import Prog ( call, Member(prj), Prog(..) )
import Data.Function (fix)

-- | Lift effect
newtype Lift m a = Lift (m a)

-- | Lift a monadic computation @m a@ into the effect @Lift m@
lift :: (Member (Lift m) es) => m a -> Prog es a
lift = call . Lift

-- | Handle @Lift m@ as the last effect
handleLift :: forall m w. Monad m => Prog '[Lift m] w -> m w
handleLift (Val x) = return x
handleLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= handleLift . q
     Nothing -> error "Impossible: Nothing cannot occur"


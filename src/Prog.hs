{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | An encoding for algebraic effects, based on the @freer@ monad. 
-}

module Prog (Prog(..), EffectSum, call, discharge, Member(..)) where

import Control.Monad ( (>=>) )
import Data.Kind (Constraint)
import FindElem ( Idx(unIdx), FindElem(..) )
import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import Unsafe.Coerce ( unsafeCoerce )

-- | A program represented as a syntax tree, whose nodes are operations and leaves are pure values
data Prog es a where
  Val 
    :: a                -- ^ pure value 
    -> Prog es a
  Op 
    :: EffectSum es x   -- ^ operation
    -> (x -> Prog es a) -- ^ continuation
    -> Prog es a

instance Functor (Prog es) where
  fmap f (Val a) = Val (f a)
  fmap f (Op fx k) = Op fx (fmap f . k)

instance Applicative (Prog es) where
  pure = Val
  Val f <*> x = fmap f x
  (Op fx k) <*> x = Op fx ((<*> x) . k)

instance Monad (Prog es) where
  return            = Val
  Val a >>= f      = f a
  Op fx k >>= f = Op fx (k >=> f)

-- | Run a pure computation
run :: Prog '[] a -> a
run (Val x) = x
run _ = error "'run' isn't defined for non-pure computations"

-- | Call an operation of type @e x@ in a computation
call :: (Member e es) => e x -> Prog es x
call e = Op (inj e) Val

-- | An open sum or union for an effect signature 'es' of effect types.
data EffectSum (es :: [* -> *]) (x :: *) :: * where
  EffectSum :: Int -> e x -> EffectSum es x

-- | Membership of an effect @e@ in @es@
class (FindElem e es) => Member (e :: * -> *) (es :: [* -> *]) where
  inj ::  e x -> EffectSum es x
  prj ::  EffectSum es x -> Maybe (e x)

instance {-# INCOHERENT #-} (e ~ e') => Member e '[e'] where
   inj x  = EffectSum 0 x
   prj (EffectSum _ x) = Just (unsafeCoerce x)

instance (FindElem e es) => Member e es where
  inj = EffectSum (unIdx (findElem :: Idx e es))
  prj = prj' (unIdx (findElem :: Idx e es))
    where prj' n (EffectSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing

-- | Membership of many effects @es@ in @ess@
type family Members (es :: [* -> *]) (ess :: [* -> *]) = (cs :: Constraint) | cs -> es where
  Members (e ': es) ess = (Member e ess, Members es ess)
  Members '[] ess       = ()


-- | Discharges an effect @e@ from the front of an effect signature @es@
discharge :: EffectSum (e ': es) x -> Either (EffectSum es x) (e x)
discharge (EffectSum 0 tv) = Right $ unsafeCoerce tv
discharge (EffectSum n rv) = Left  $ EffectSum (n-1) rv

-- | For pattern-matching against operations that belong in the tail of an effect signature
pattern Other :: EffectSum es x -> EffectSum  (e ': es) x
pattern Other u <- (discharge -> Left u)

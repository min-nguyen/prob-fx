{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenSum where

import Data.Typeable
import Data.Kind (Type, Constraint)
import Data.Proxy
-- import Fcf
import GHC.Natural
import GHC.TypeLits (Nat, KnownNat, natVal, TypeError, ErrorMessage (Text, (:$$:), (:<>:), ShowType))
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import FindElem

data OpenSum (as :: [k]) where
  UnsafeOpenSum :: Int -> a -> OpenSum as

instance Eq (OpenSum '[]) where
  x == _ = case x of {}

instance forall a as. (Eq a, Eq (OpenSum as)) => Eq (OpenSum (a : as)) where
  UnsafeOpenSum i _ == UnsafeOpenSum j _ | i /= j = False
  UnsafeOpenSum 0 x == UnsafeOpenSum 0 y =
    unsafeCoerce x == (unsafeCoerce y :: a)
  UnsafeOpenSum i x == UnsafeOpenSum j y =
    UnsafeOpenSum (i - 1) x == (UnsafeOpenSum (j - 1) y :: OpenSum as)

instance forall a as. (Show a, Show (OpenSum as)) => Show (OpenSum (a : as)) where
  show (UnsafeOpenSum i a) =
    if i == 0
    then show (unsafeCoerce a :: a)
    else show (UnsafeOpenSum (i - 1) a :: OpenSum as)

instance {-# OVERLAPPING #-} Show a => Show (OpenSum '[a]) where
  show (UnsafeOpenSum i a) = show (unsafeCoerce a :: a)

class (FindElem a as) => Member (a :: *) (as :: [*]) where
  inj ::  a -> OpenSum as
  prj ::  OpenSum as  -> Maybe a

instance (Typeable a, a ~ a') => Member a '[a'] where
   inj x          = UnsafeOpenSum 0 x
   prj (UnsafeOpenSum _ x) = Just (unsafeCoerce x)

instance (FindElem a as) => Member a as where
  inj = UnsafeOpenSum (unP (findElem :: P a as))
  prj = prj' (unP (findElem :: P a as))
    where prj' n (UnsafeOpenSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing

type family Members (as :: [* ]) (tss :: [* ]) = (cs :: Constraint) | cs -> as where
  Members (a ': as) tss = (Member a tss, Members as tss)
  Members '[] tss       = ()
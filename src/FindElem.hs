{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Auxiliary definitions for finding a type in a type-level list.
-}

module FindElem (
    FindElem(..)
  , Idx(..)) where

import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )

-- | Proof that @x@ is an element of the type-level list @xs@
class FindElem x xs where
  findElem :: Idx x xs

-- | The integer index of @x@ in @xs@
newtype Idx x xs = Idx {unIdx :: Int}

instance FindElem x (x ': xs) where
  findElem = Idx 0

instance {-# OVERLAPPABLE #-} FindElem x xs => FindElem x (x' : xs) where
  findElem = Idx $ 1 + unIdx (findElem :: Idx x xs)

instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType x ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem x '[] where
  findElem = error "unreachable"
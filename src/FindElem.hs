{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FindElem (FindElem(..), P(..)) where

import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )

-- | Auxiliary type-class for proof that @t@ is an element of the type-level list @ts@
class FindElem t ts where
  findElem :: P t ts

newtype P t ts = P {unP :: Int}

instance FindElem t (t ': ts) where
  findElem = P 0

instance {-# OVERLAPPABLE #-} FindElem t ts => FindElem t (t' : ts) where
  findElem = P $ 1 + unP (findElem :: P t ts)

instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  findElem = error "unreachable"
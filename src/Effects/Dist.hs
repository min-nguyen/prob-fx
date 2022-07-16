{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

{- | The effect @Dist@ for primitive distributions
-}

module Effects.Dist (
  -- ** Address
    Tag
  , Addr
  -- ** Dist effect
  , Dist(..)
  , handleDist
  -- ** Sample effect
  , Sample(..)
  -- ** Observe effect
  , Observe(..)
  ) where

import Data.Map (Map)
import Data.Maybe ( fromMaybe )
import Prog ( call, discharge, Member, Prog(..) )
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import Util ( boolToInt )
import PrimDist ( PrimDist )

-- | An observable variable name assigned to a primitive distribution
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence
type Addr = (Tag, Int)

-- | Distribution effect
data Dist a = Dist
  { getPrimDist :: PrimDist a  -- ^ primitive distribution
  , getObs :: Maybe a          -- ^ optional observed value
  , getTag :: Maybe Tag        -- ^ optional observable variable name
  }

instance Show a => Show (Dist a) where
  show (Dist d y tag) = "Dist(" ++ show d ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

instance Eq (Dist a) where
  (==) (Dist d1 _ _) (Dist d2 _ _) = d1 == d2

-- | The effect for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist a     -- ^ Distribution to sample from
          -> Addr           -- ^ Address of @Sample@ operation
          -> Sample a

-- | The effect for conditioning against observed values
data Observe a where
  Observe :: PrimDist a     -- ^ Distribution to condition with
          -> a              -- ^ Observed value
          -> Addr           -- ^ Address of @Observe@ operation
          -> Observe a

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign an address
handleDist :: (Member Sample es, Member Observe es)
  => Prog (Dist : es) a
  -> Prog es a
handleDist = loop 0 Map.empty
  where
  loop :: (Member Sample es, Member Observe es)
       => Int -> Map Tag Int -> Prog (Dist : es) a -> Prog es a
  loop _ _ (Val x) = return x
  loop counter tagMap (Op u k) = case discharge u of
    Right (Dist d maybe_y maybe_tag) ->
         case maybe_y of
              Just y  -> do call (Observe d y (tag, tagIdx)) >>= k'
              Nothing -> do call (Sample d (tag, tagIdx))    >>= k'
          where tag     = fromMaybe (show counter) maybe_tag
                tagIdx  = Map.findWithDefault 0 tag tagMap
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop (counter + 1) tagMap' . k
    Left  u'  -> Op u' (loop counter tagMap . k)


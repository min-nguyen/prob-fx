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
module Effects.Dist where

import Data.Kind
import Data.Map (Map)
import Data.Maybe
import Numeric.Log
import Prog ( call, discharge, Member, Prog(..) )
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum as OpenSum
import Sampler
import Statistics.Distribution
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.Dirichlet
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Gamma
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson
import Statistics.Distribution.Uniform
import Util ( boolToInt )
import PrimDist

-- ||| (Section 4.2.1) Effects for distributions
-- | The Dist effect has a single operation `Dist` that takes as arguments: a primitive distribution of type `PrimDist a`, an optional observed value of type `Maybe a`, and an optional observable variable name of type `Maybe String` .
data Dist a = Dist { getPrimDist :: PrimDist a, getObs :: Maybe a, getTag :: Maybe String}

-- ||| (Section 5.3) Handling Distributions
data Sample a where
  Sample  :: PrimDist a -> Addr -> Sample a

data Observe a where
  Observe :: PrimDist a -> a -> Addr -> Observe a

-- | Interpret Dist to Sample or Observe, and add address
handleDist :: (Member Sample es, Member Observe es)
        => Prog (Dist : es) a -> Prog es a
handleDist = loop 0 Map.empty
  where
  loop :: (Member Sample es, Member Observe es)
       => Int -> TagMap -> Prog (Dist : es) a -> Prog es a
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

type Tag  = String
type Addr = (Tag, Int)
type TagMap = Map Tag Int

-- | For constraining the output types of distributions
instance Show a => Show (Dist a) where
  show (Dist d y tag) = "Dist(" ++ show d ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

instance Eq (Dist a) where
  (==) (Dist d1 _ _) (Dist d2 _ _) = d1 == d2 
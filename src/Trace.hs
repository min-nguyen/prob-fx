{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Trace where

import Data.Map (Map)
import Data.Maybe
import Effects.Dist
import Env
import GHC.TypeLits
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified OpenSum as OpenSum
import Util

-- | Sample trace, mapping addresses of sample/observe operations to their distributions and sampled values
type STrace = Map Addr (PrimDist, OpenSum PrimVal)

type Trace a = [(a, STrace)]

-- | For converting sample traces, as used by simulation and inference, to output model environments
class FromSTrace a where
  fromSTrace :: STrace -> Env a

instance FromSTrace '[] where
  fromSTrace _ = nil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

updateSTrace :: (Show x, OpenSum.Member x PrimVal)
  => Addr -> Dist x -> x -> STrace -> STrace
updateSTrace α d x = Map.insert α (PrimDist d, OpenSum.inj x)

-- | Log probability trace, mapping addresses of sample/observe operations to their log probabilities
type LPTrace = Map Addr Double

updateLPTrace :: Addr -> Dist x -> x -> LPTrace -> LPTrace
updateLPTrace α d x = Map.insert α (logProb d x)

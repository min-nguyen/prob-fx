{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Freer.Trace where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import GHC.TypeLits ( KnownSymbol )
import Freer.Effects.Dist
import Env
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util

type STrace = Map Addr (PrimDist, OpenSum PrimVal)

type Trace a = [(a, STrace)]

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

type LPTrace = Map Addr Double

updateLPTrace :: Addr -> Dist x -> x -> LPTrace -> LPTrace
updateLPTrace α d x = Map.insert α (logProb d x)

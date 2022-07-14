{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Trace (STrace, LPTrace, FromSTrace(..), updateSTrace, updateLPTrace) where

import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import Effects.Dist ( Addr )
import PrimDist ( ErasedPrimDist(..), PrimVal, PrimDist, logProb )
import Env ( UniqueKey, Assign((:=)), Env(ECons), ObsVar(..), varToStr, nil )
import GHC.TypeLits ( KnownSymbol )
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified OpenSum

-- ** (Section 6.1) Sample trace 
-- | The type of sample traces, mapping addresses of sample/observe operations to their primitive distributions and sampled values
type STrace = Map Addr (ErasedPrimDist, OpenSum PrimVal)

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
  => Addr -> PrimDist x -> x -> STrace -> STrace
updateSTrace α d x = Map.insert α (ErasedPrimDist d, OpenSum.inj x)

-- ** (Section 6.2.2) Log-probability trace 
-- | The type of log-probability traces, mapping addresses of sample/observe operations to their log probabilities
type LPTrace = Map Addr Double

updateLPTrace :: Addr -> PrimDist x -> x -> LPTrace -> LPTrace
updateLPTrace α d x = Map.insert α (logProb d x)

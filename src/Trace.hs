{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | For recording samples and log-probabilities during model execution 
-}

module Trace (
  -- * Sample trace
    STrace
  , FromSTrace(..)
  , updateSTrace
  -- * Log-probability trace
  , LPTrace
  , updateLPTrace) where

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

-- | The type of sample traces, mapping addresses of sample/observe operations to their primitive distributions and sampled values
type STrace = Map Addr (ErasedPrimDist, OpenSum PrimVal)

-- | For converting sample traces, as used by simulation and inference, to output model environments
class FromSTrace env where 
  -- | Convert a sample trace to a model environment
  fromSTrace :: STrace -> Env env

instance FromSTrace '[] where
  fromSTrace _ = nil
 
instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

-- | Update a sample trace at an address
updateSTrace :: (Show x, OpenSum.Member x PrimVal) =>
  -- | Address of sample site
     Addr       
  -- | Primitive distribution at address
  -> PrimDist x 
  -- | Sampled value
  -> x 
  -- | Previous sample trace
  -> STrace 
  -- | Updated sample trace
  -> STrace
updateSTrace α d x = Map.insert α (ErasedPrimDist d, OpenSum.inj x)

-- | The type of log-probability traces, mapping addresses of sample/observe operations to their log probabilities
type LPTrace = Map Addr Double

-- | Compute and update a log-probability trace at an address
updateLPTrace :: 
  -- | Address of sample/observe site
     Addr 
  -- | Primitive distribution at address
  -> PrimDist x 
  -- | Sampled or observed value
  -> x 
  -- | Prevous log-prob trace
  -> LPTrace 
  -- | Updated log-prob trace
  -> LPTrace
updateLPTrace α d x = Map.insert α (logProb d x)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | The effects for primitive distributions, sampling, and observing.
-}

module Effects.Dist (
  -- ** Address
  -- $Address
    Tag
  , Addr
  -- ** Dist effect
  , Dist(..)
  , handleDist
  -- ** Sample effect
  , Sample(..)
  , pattern Samp
  -- ** Observe effect
  , Observe(..)
  , pattern Obs
  ) where

import Data.Map (Map)
import Data.Maybe ( fromMaybe )
import Prog ( call, discharge, Member(..), Prog(..), EffectSum(..) )
import qualified Data.Map as Map
import PrimDist ( PrimDist )

{- $Address
   Run-time identifiers for probabilistic operations
-}

-- | An observable variable name assigned to a primitive distribution, representing a compile-time identifier
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence, representing a run-time identifier
type Addr = (Tag, Int)

-- | The effect @Dist@ for primitive distributions
data Dist a = Dist
  { getPrimDist :: PrimDist a  -- ^ primitive distribution
  , getObs :: Maybe a          -- ^ optional observed value
  , getTag :: Maybe Tag        -- ^ optional observable variable name
  }

instance Show a => Show (Dist a) where
  show (Dist d y tag) = "Dist(" ++ show d ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

instance Eq (Dist a) where
  (==) (Dist d1 _ _) (Dist d2 _ _) = d1 == d2

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist a     -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern Samp :: Member Sample es => PrimDist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj  -> Just (Sample d α))

-- | The effect @Observe@ for conditioning against observed values
data Observe a where
  Observe :: PrimDist a     -- ^ distribution to condition with
          -> a              -- ^ observed value
          -> Addr           -- ^ address of @Observe@ operation
          -> Observe a

-- | For projecting and then successfully pattern matching against @Observe@
pattern Obs :: Member Observe es => PrimDist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

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


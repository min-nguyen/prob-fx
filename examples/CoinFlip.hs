{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

{- | A coin-flip model for demonstrating how primitive distributions work in ProbFX.
-}

module CoinFlip where

import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), bernoulli, uniform )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables )

{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
coinFlip
  :: (Observables env '["p"] Double
    , Observables env '[ "y"] Bool)
  => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

{- | A desugared version of the above coin-flip model, after inlining the functions
     @uniform@ and @bernoulli@.
-}
coinFlip' :: forall env es. (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip' = Model $ do
  maybe_p  <- call (Ask @env #p)
  p        <- call (Dist (UniformDist 0 1) maybe_p (Just "p"))
  maybe_y  <- call (Ask @env #y)
  y        <- call (Dist (BernoulliDist p) maybe_y (Just "p") )
  return y

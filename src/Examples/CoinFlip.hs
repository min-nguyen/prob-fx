{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Examples.CoinFlip where

import Prog
import Effects.ObsReader
import Model
import Effects.Dist
import Data.Kind (Constraint)
import Env

-- | Coin flip model

coinFlip :: (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

-- | Desugared coin flip model

coinFlip' :: forall env es. (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip' = Model $ do
  maybe_p  <- call (Ask @env #p)
  p        <- call (UniformDist 0 1 maybe_p (Just "p"))
  maybe_y  <- call (Ask @env #y)
  y        <- call (BernoulliDist p maybe_y (Just "p") )
  return y

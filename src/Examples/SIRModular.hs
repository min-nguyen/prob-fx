{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Examples.SIRModular where

import qualified Data.Extensible as Extensible
import Data.Extensible hiding (Member)
import Prog
import Control.Lens hiding ((:>))
import Effects.Writer
import Model
import Control.Monad
import Env
import Examples.HMM
import Data.Extensible (Associated)
import GHC.TypeLits
import Data.Kind (Constraint)

type family Lookups env (ks :: [Symbol]) a :: Constraint where
  Lookups env (x ': xs) a = (Lookup env x a, Lookups env xs a)
  Lookups env '[] a = ()

mkField "s i r v"

type Reported = Int

{- SIR model using extensible records -}

-- | SIR transition model
transSI :: Lookups popl '["s", "i", "r"] Int => TransModel env ts Double (Record popl)
transSI  beta popl = do
  let (s_0, i_0, r_0 ) = (popl ^. s,  popl ^. i,  popl ^. r)
      pop = s_0 + i_0 + r_0
  dN_SI <- binomial' s_0 (1 - exp ((-beta * fromIntegral i_0) / fromIntegral pop))
  return $ popl & s .~ (s_0 - dN_SI)
                & i .~ (i_0 + dN_SI)

transIR :: Lookups popl '["i", "r"] Int => TransModel env ts Double (Record popl)
transIR  gamma popl = do
  let (i_0, r_0) = (popl ^. i,  popl ^. r)
  dN_IR <- binomial' i_0 (1 - exp (-gamma))
  return $ popl & i .~ (i_0 - dN_IR)
                & r .~ (r_0 + dN_IR)

transSIR :: (Member (Writer [Record popl]) ts, Lookups popl '["s", "i", "r"] Int)
  => TransModel env ts (Double, Double) (Record popl)
transSIR (beta, gamma) popl = do
  popl <- (transSI beta >=> transIR gamma) popl
  tellM [popl]
  return popl

-- | SIR observation model
type ObsParams = Double

obsSIR :: Lookup s "i" Int => Observable env "𝜉" Int
  => ObsModel env ts Double (Record s) Reported
obsSIR rho popl  = do
  let i_0 = popl ^. i
  poisson (rho * fromIntegral i_0) #𝜉

-- | SIR transition prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env ts (Double, Double)
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (pBeta, pGamma)

-- | SIR observation prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env ts ObsParams
obsPriorSIR = beta 2 7 #ρ

-- | SIR as HMM
hmmSIR :: (Member (Writer [Record popl]) es,
           Lookups popl '["s", "i", "r"] Int, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIR n  = handleWriterM . hmmGen transPriorSIR obsPriorSIR transSIR obsSIR n

{- SIRS (resusceptible) model -}

-- | SIRS transition model
transRS :: Lookups popl '["s", "r"] Int => TransModel env ts Double (Record popl)
transRS eta popl = do
  let (r_0, s_0) = (popl ^. r,  popl ^. s)
  dN_RS <- binomial' r_0 (1 - exp (-eta))
  return $ popl & r .~ (r_0 - dN_RS)
                & s .~ (s_0 + dN_RS)

transSIRS :: Lookups popl '["s", "i", "r"] Int => TransModel env es (Double, Double, Double) (Record popl)
transSIRS (beta, gamma, eta) = transSI beta >=> transIR gamma >=> transRS eta

-- | SIRS transition prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env ts (Double, Double, Double)
transPriorSIRS = do
  (pBeta, pGamma)  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (pBeta, pGamma, pEta)

-- | SIRS as HMM
hmmSIRS :: (Member (Writer [Record popl]) es,
            Lookups popl '["s", "i", "r"] Int,
            Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIRS n = handleWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

{- SIRSV (resusceptible + vacc) model -}

-- | SIRSV transition model
transSV :: Lookups popl '["s", "v"] Int => TransModel env es Double (Record popl)
transSV omega popl  = do
  let (s_0, v_0) = (popl ^. s,  popl ^. v)
  dN_SV <- binomial' s_0 (1 - exp (-omega))
  return $ popl & s .~ (s_0 - dN_SV)
                & v .~ (v_0 + dN_SV)

transSIRSV :: Lookups popl '["s", "i", "r", "v"] Int => TransModel env ts (Double, Double, Double, Double) (Record popl)
transSIRSV (beta, gamma, eta, omega) =
  transSI beta >=> transIR gamma >=> transRS eta  >=> transSV omega

-- | SIRSV transition prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env ts (Double, Double, Double, Double)
transPriorSIRSV  = do
  (pBeta, pGamma, pEta) <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (pBeta, pGamma, pEta, pOmega)

-- | SIRSV as HMM
hmmSIRSV :: (Member (Writer [Record popl]) es,
             Lookups popl '["s", "i", "r", "v"] Int,
             Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ω", "ρ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIRSV n = handleWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIR n
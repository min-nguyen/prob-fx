{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Examples.SIR where

import Prog
import Effects.Writer
import Model
import Inference.Simulate as Simulate
import Inference.MH as MH
import Sampler
import Env
import Control.Monad

import Examples.HMM
import Data.Extensible (Associated)

{--- SIR model (Sec 3.1) ---}
data Popl = Popl {
    s   :: Int, -- ^ Number of people susceptible to infection
    i   :: Int, -- ^ Number of people currently infected
    r   :: Int  -- ^ Number of people recovered from infection
} deriving Show

type Reported = Int

-- | SIR transition model
transSI :: TransModel env ts Double Popl
transSI beta (Popl s i r) = do
  let pop = s + i + r
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ Popl (s - dN_SI) (i + dN_SI) r

transIR :: TransModel env ts Double Popl
transIR gamma (Popl s i r)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ Popl s (i - dN_IR) (r + dN_IR)

data TransParamsSIR = TransParamsSIR {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ Mean recovery rate
}

transSIR :: Member (Writer [Popl]) es
  => TransModel env es TransParamsSIR Popl
transSIR (TransParamsSIR beta gamma) sir = do
  sir' <- (transSI beta >=> transIR gamma) sir
  tellM [sir']
  return sir'

-- | SIR observation model
type ObsParams = Double

obsSIR :: Observable env "𝜉" Int
  => ObsModel env ts Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIR transition prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env ts TransParamsSIR
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (TransParamsSIR pBeta pGamma)

-- | SIR observation prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env ts ObsParams
obsPriorSIR = do
  pRho <- beta 2 7 #ρ
  return pRho

-- | SIR as HMM
hmmSIR :: (Member (Writer [Popl]) es, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env es Popl
hmmSIR = hmmGen transPriorSIR obsPriorSIR transSIR obsSIR

hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env es (Popl, [Popl])
hmmSIR' n = handleWriterM . hmmSIR n

type SIRenv = '["β" := Double, "γ"  := Double, "ρ"  := Double, "𝜉" := Int]

-- | Simulate from SIR model: ([(s, i, r)], [𝜉])
simulateSIR :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIR = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- Simulate.simulate (hmmSIR' 100) sim_env_in sir_0
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

-- | Infer from SIR model: ([ρ], [β])
inferSIR :: Sampler ([Double], [Double])
inferSIR = do
  𝜉s <- snd <$> simulateSIR
  let mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> nil
      sir_0           = Popl {s = 762, i = 1, r = 0}
  mhTrace <- MH.mh 50000 (hmmSIR' 100) (sir_0, mh_env_in) ["β", "ρ"]
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)



{- (3.2.1) EXTENSIONS TO SIR MODEL:
Note that the implementations below aren't as modular as we would like, due to having to redefine the data types Popl and TransParams when adding new variables to the SIR model. The file "src/Examples/SIRModular.hs" shows how one could take steps to resolve this by using extensible records.
-}


{--- SIRS (resusceptible) model ---}
data TransParamsSIRS = TransParamsSIRS {
    betaP_SIRS  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP_SIRS :: Double, -- ^ Mean recovery rate
    etaP_SIRS   :: Double  -- ^ Rate of resusceptible
}

-- | SIRS transition model
transRS :: Double -> Popl -> Model env ts Popl
transRS eta (Popl s i r) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ Popl (s + dN_RS) i (r - dN_RS)

transSIRS :: Member (Writer [Popl]) es
  => TransModel env es TransParamsSIRS Popl
transSIRS (TransParamsSIRS beta gamma eta) sir = do
  sir' <- (transSI beta >=> transIR gamma >=> transRS eta) sir
  tellM [sir']
  return sir'

-- | SIR transition prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env ts TransParamsSIRS
transPriorSIRS = do
  TransParamsSIR pBeta pGamma  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (TransParamsSIRS pBeta pGamma pEta)

-- | SIRS as HMM
hmmSIRS :: (Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double) => Int -> Popl -> Model env ts (Popl, [Popl])
hmmSIRS n = handleWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simulateSIRS :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIRS = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- Simulate.simulate (hmmSIRS 100) sim_env_in sir_0
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)


{--- SIRSV (resusceptible + vacc) model ---}
data TransParamsSIRSV = TransParamsSIRSV {
    betaP_SIRSV  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP_SIRSV :: Double, -- ^ Mean recovery rate
    etaP_SIRSV   :: Double, -- ^ Rate of resusceptible
    omegaP_SIRSV :: Double  -- ^ Vaccination rate
}

data PoplV = PoplV {
    s' :: Int,
    i' :: Int,
    r' :: Int,
    v' :: Int
} deriving Show

-- | SIRSV transition models
transSI' :: TransModel env ts Double PoplV
transSI' beta (PoplV s i r v) = do
  let pop = s + i + r + v
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ PoplV (s - dN_SI) (i + dN_SI) r v

transIR' :: TransModel env ts Double PoplV
transIR' gamma (PoplV s i r v)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ PoplV s (i - dN_IR) (r + dN_IR) v

transRS' :: TransModel env es Double PoplV
transRS' eta (PoplV s i r v) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ PoplV (s + dN_RS) i (r - dN_RS) v

transSV' :: TransModel env es Double PoplV
transSV' omega (PoplV s i r v)  = do
  dN_SV <- binomial' s (1 - exp (-omega))
  return $  PoplV (s - dN_SV) i r (v + dN_SV )

transSIRSV :: Member (Writer [PoplV]) ts => TransModel env ts TransParamsSIRSV PoplV
transSIRSV (TransParamsSIRSV beta gamma omega eta) sirv = do
  sirv' <- (transSI' beta  >=>
            transIR' gamma >=>
            transRS' eta   >=>
            transSV' omega) sirv
  tellM [sirv']
  return sirv'

-- | SIRSV transition prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env ts TransParamsSIRSV
transPriorSIRSV  = do
  TransParamsSIRS pBeta pGamma pEta <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (TransParamsSIRSV pBeta pGamma pEta pOmega)

-- | SIRSV observation model
obsSIRSV :: Observable env "𝜉" Int
  => ObsModel env ts Double PoplV Reported
obsSIRSV rho (PoplV _ i _ v)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIRSV as HMM
hmmSIRSV ::  (Observables env '["𝜉"] Int, Observables env '["β", "γ", "η", "ω", "ρ"] Double) => Int -> PoplV -> Model env ts (PoplV, [PoplV])
hmmSIRSV n = handleWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIRSV n

-- | Simulate from SIRSV model : ([(s, i, r, v)], [𝜉])
simulateSIRSV :: Sampler ([(Int, Int, Int, Int)], [Reported])
simulateSIRSV = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
      sirv_0      = PoplV {s' = 762, i' = 1, r' = 0, v' = 0}
  ((_, sirv_trace), sim_env_out) <- Simulate.simulate (hmmSIRSV 100) sim_env_in sirv_0
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirvs = map (\(PoplV s i recov v) -> (s, i, recov, v)) sirv_trace
  return (sirvs, 𝜉s)
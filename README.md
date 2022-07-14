### ProbFX:    [**Modular Probabilistic Models via Algebraic Effects**](https://github.com/min-nguyen/prob-fx/blob/master/paper.pdf)


#### Interacting with ProbFX

To interact with the many example models of ProbFX and visualise their results in a plot, please see https://github.com/min-nguyen/prob-fx which provides and documents an executable version of ProbFX with a script!

#### Description
ProbFX is a library for probabilistic programming using algebraic effects that implements the paper [**Modular Probabilistic Models via Algebraic Effects**](https://github.com/min-nguyen/prob-fx/blob/master/paper.pdf) -- this provides a comprehensive motivation and walkthrough of this library. In short, the emphasis is on being able to define modular and reusable probabilistic models, and secondly, compositional implementation of model execution (inference) in terms of effect handlers. 

**Paper to artifact overview**

- § 1: Linear regression `(src/Examples/LinRegr.hs)`
  - Simulating  (Fig 1a) is done via `./prob-fx.sh simLinRegr`.
  - Likelihood weighting inference (Fig 1b) is done via `./prob-fx.sh lwLinRegr`.

- § 2: Hidden Markov model `(src/Examples/HMM.hs)`
  - You can find both the loop HMM version (Fig 2) and the modular HMM version (Fig 3) in here.

- § 3: SIR model (`src/Examples/SIR.hs`)
  - § 3.1 The SIR model is the function `hmmSIR'`; the higher-order HMM function it uses is imported from `HMM.hs`, and the `Writer` effect it uses from §5.5 is already integrated.
    - Simulating from this (Fig 4a) can be done via `./prob-fx.sh simSIR`.
    - Metropolis-Hastings inference (Fig 5) is done via `./prob-fx.sh mhSIR`; this takes 1-3 minutes
  - § 3.2.1: The _resusceptible_ extension is defined as `hmmSIRS`, and the _resusceptible + vaccinated_ version is `hmmSIRSV`.

    - Simulating from `hmmSIRS` (Fig 4b) is done via `./prob-fx.sh simSIRS`
    - Simulating from `hmmSIRSV` (Fig 4c) is done via `./prob-fx.sh simSIRSV`.

    Note that their implementations are not as modular as we would like (due to having to redefine the data types `Popl` and `TransParams`) -- the program `SIRModular.hs` shows how one could take steps to resolve this by using extensible records.

- § 4: Embedding
  - § 4.1: The definition of `Prog` is in `src/Prog.hs`, including auxiliary types and functions.
  - § 4.2: The definition of `Model` and the smart constructors for primitive distributions are in `src/Model.hs`.
    The effect type `Dist` (§ 4.2.1) is split up into `src/Effects/Dist.hs` and `src/PrimDist.hs`. The effect `ObsReader` (§ 4.2.2) is in `src/Effects/ObsReader.hs`.

- § 5: Interpreting multimodal models
  - Intro: Coinflip example: `src/Examples/CoinFlip.hs`
  - § 5.1: Model environments: `src/Env.hs`.
  - § 5.2: Handling `ObsReader`: `src/Effects/ObsReader.hs`.
  - § 5.3: Handling `Dist`: `src/Effects/Dist.hs`.
  - § 5.4: Specialising multimodal models: `src/Model.hs`.
  - § 5.5: Extending models with extra effects: `src/Examples/SIR.hs`

- § 6: Simulation and inference as effect handlers
  - § 6.1: Simulation: `src/Inference/SIM.hs`. The type of `STrace` is in `src/Trace.hs`.
  - § 6.2.1: Likelihood Weighting: `src/Inference/LW.hs`.
  - § 6.2.2: Metropolis Hastings: `src/Inference.MH.hs`.

#### Examples

We provide and document a large number of examples of defining and then executing probabilistic models, which can be found in `Examples`. [`LogRegr.hs`](https://github.com/min-nguyen/prob-fx/blob/master/src/Examples/LogRegr.hs) documents a particularly representative walk-through. The general process of doing this is as follows:
1. Define an appropriate model of type `Model env es a` and a corresponding model environment of type `Env env`.
2. Execute a model using one of the library functions `simulate`, `lw`, or `mh` detailed in `src/Inference`; this produces an output in the monad `Sampler`.
3. `Sampler` computations can be evaluated with `sampleIO` (found in `src/Sampler.hs`) to produce an `IO` computation. Examples of this are shown in `Main.hs`.

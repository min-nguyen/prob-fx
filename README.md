## ProbFX Artifact: [**Modular Probabilistic Models via Algebraic Effects**](https://github.com/min-nguyen/prob-fx/blob/main/paper.pdf)

---

### Dependencies

_Running ProbFX_

ProbFX uses Cabal version `3.6` and a GHC version `>= 8.4.1` and `<= 9.0.2`:
1. Install GHCup by following [these instructions](https://www.haskell.org/ghcup/install/)
2. Both Cabal and GHC versions can be installed and then set by following [this](https://www.haskell.org/ghcup/guide/).

_Visualising ProbFX_

Visualising the provided examples of ProbFX requires Python3 with the following Python packages:
- `ast`, `matplotlib`, `scipy`, `sklearn`, `numpy`

---

### Interacting with ProbFX

#### Directly executing ProbFX

We provide a script below for executing ProbFX with a set of possible arguments. Alternatively, you can:
  1. Directly execute a ProbFX program via `cabal run prob-fx <arg>` (corresponding to `Main.hs`), whose output will be written to `model-output.txt`
  2. Visualise this output as a graph via `python3 graph.py <arg>`, which will be saved to `model-output.pdf` .

#### Executing ProbFX with the script

The script **`./prob-fx.sh <arg>`** will execute a ProbFX program and save the output graph to `model-output.pdf` (the results of simulation and inference will of course vary across different executions).

The examples in the paper can be run with one of the following arguments (e.g. `./prob-fx.sh simSIR`), where the prefix `sim` denotes simulation, `lw` denotes Likelihood-Weighting inference, and `mh` denotes Metropolis-Hastings inference:
- Linear regression
  - `simLinRegr`: (Fig 1a)
  - `lwLinRegr` : (Fig 1b)
- SIR model
  - `simSIR`    : (Fig 4a)
  - `simSIRS`   : (Fig 4b) SIR + resusceptible model
  - `simSIRSV`  : (Fig 4c) SIR + resusceptible + vacc model
  - `mhSIR`     : (Fig 5) this takes 2-3 minutes

Extra example models and their corresponding command-line arguments are given below:
- Linear regression:
  - `mhLinRegr`  : This generates the approximative posterior distribution over the model parameter `mu`, being the gradient of the linear relationship.
- Logistic regression, which models the occurrence of one event out of two alternatives happening:
  - `simLogRegr`
  - `lwLogRegr`  : This generates a likelihood distribution over the model parameter `mu`.
  - `mhLogRegr`  : This generates an approximative posterior distribution over the model parameters `mu` and `b`.
- Latent dirichlet allocation, i.e. a [topic model](https://www.tidytextmining.com/topicmodeling.html) modeling the distribution over topics and words in a text document. We assume the example vocabulary "DNA", "evolution", "parsing", and "phonology" for text documents, and we assume there are two possible topics.
  - `simLDA`     : This simulates a document of unstructured words from the vocabulary according.
  - `mhLDA`      : Given a pre-defined document of words, the generated graph shows a predictive posterior distribution over the two topics occurring in the document, and the distribution over the words occurring in each topic.
- A [case study](https://docs.pymc.io/en/v3/pymc-examples/examples/case_studies/multilevel_modeling.html) by Gelman and Hill as a hierarchical linear regression model, modelling the relationship between radon levels in households and whether these houses contain basements:
  - `simRadon` : This simulates the log-radon levels of houses with basements and those without basements.
  - `mhRadon`  : This genereates the predictive posterior distribution over "gradients" for each county; each gradient models the relationship of log-radon levels of houses with and without basements in that county.
- Another Gelman and Hill [case study](https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html) as a hierarchical model, which quantifies the effect of coaching programs from 8 different schools on students' SAT-V scores:
  - `mhSchool` : This generates a posterior distribution over model parameter `mu`, being the effect of general coaching programs on SAT scores, and each school's posterior distribution on model parameter `theta`, being the variation of their effect on SAT scores.

#### Example models + Creating and executing models

All example models can be found in `src/Examples`, showing variations on how models can be created and executed. [`LogRegr.hs`](https://github.com/min-nguyen/prob-fx/blob/artifact/src/Examples/LogRegr.hs) documents a particularly representative walk-through. The general process of doing this is as follows:
1. Define an appropriate model of type `Model env es a` and a corresponding model environment of type `Env env`.
2. Execute a model using one of the library functions `simulate`, `lw`, or `mh` detailed in `src/Inference`; this produces an output in the monad `Sampler`.
3. `Sampler` computations can be evaluated with `sampleIO` (found in `src/Sampler.hs`) to produce an `IO` computation. Examples of this are shown in `Main.hs`.

---

### Paper to artifact overview

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

---

## Benchmarks

To compare benchmarks with MonadBayes and Turing:

- _MonadBayes_ uses Cabal version `3.6`, and requires a GHC version of `>= 8.4` and `< 8.10`. To execute both ProbFX and MonadBayes in the same environment, we suggest GHC version `8.6.5`.
- _Turing_ requires the language [Julia](https://julialang.org/downloads/) to be installed. After doing so, the necessary packages can be installed via:
    ```
    julia
    import Pkg
    Pkg.add("Turing")
    Pkg.add("BenchmarkTools")
    Pkg.add("DataFrames")
    Pkg.add("CSV")
    Pkg.add("Statistics")
    ```

### Directly benchmarking ProbFX

Benchmarking is done with a version of ProbFX that uses the `freer-simple` library as an algebraic effect encoding; this is found in `src/Freer`. We provide a script for benchmarking below, but ProbFX can also be directly benchmarked with `cabal run benchmarking-prob-fx` (corresponding to `benchmarking/prob-fx/Main.hs`). The examples programs used for benchmarking are found in `benchmarking/prob-fx/BenchmarkPrograms.hs`, and they are benchmarked in `benchmarking/prob-fx/BenchmarkTests.hs`.

### Benchmarking with the script

The benchmarks in Appendix A can be reproduced by running the script **`./benchmark.sh <arg>`** with one the following arguments:
- `prob-fx`    : This writes to `benchmarking/prob-fx/prob-fx-benchmarks.csv`
- `monad-bayes` : This writes to `benchmarking/monad-bayes/monad-bayes-benchmarks.csv`
- `turing`      : This writes to `benchmarking/turing/turing-benchmarks.csv`

These will run a given language's benchmarks in the order shown in Appendix A (Figure 8 and then Figure 9), from left to right, and top to bottom.

Each benchmark is given a name of the format `<model>/<algorithm>/<independent-variable>/<value>` where:
- `<model>` is the model shown on the LHS Y-axis of the figures
- `<algorithm>` is the model execution algorithm shown on the top of the figures
- `<independent-variable>` is the variable being varied over on the bottom of the figures: this can be `sample-size` which corresponds to Fig 8, or `data-size` corresponding to Fig 9.
- `<value>` is the value assigned to the `<independent-variable>`.

For example, `lda/MH/data-size/80` means a latent-dirichlet model is executed under Metropolis-Hastings for a dataset size of 80.

### Creating new ProbFX benchmarks

We use Criterion to benchmark ProbFX; a tutorial can be found [here](http://www.serpentine.com/criterion/tutorial.html).

A helper function for benchmarking ProbFX programs is provided in `benchmarking-prob-fx/BenchmarkTests.hs`,
```
benchmark :: forall a. NFData a
  => String                     -- benchmark group name
  -> (Int -> Int -> Sampler a)  -- sample size -> dataset size -> Sampler computation
  -> [(String, (Int, Int))]     -- [(benchmark label, (sample size, dataset set))]
  -> IO ()
benchmark groupName benchmarkProg params = ...
```
For example, below benchmarks a program `simLinRegr` with sample and data sizes `[(200, 100), (300, 100), (400, 100)]`.
```
simLinRegr :: Int        -- sample size
           -> Int        -- dataset size
           -> Sampler Double

benchmark "linear regression simulation" simLinRegr
    [(show sample_size ++ "," ++ show data_size, (sample_size, data_size))
    | (samplesize, datasize) <- [(200, 100), (300, 100), (400, 100)]]
```

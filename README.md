# ICFP 2022 Artifact

Name:    [**Modular Probabilistic Models via Algebraic Effects**](https://github.com/min-nguyen/wasabaye/blob/master/paper.pdf)

## Artifact Instructions

**Dependencies (for source only)**

-- _Wasabaye_ --

Wasabaye uses the most recent Cabal version `3.6.x` and a GHC version `>= 8.4.x`:
1. Install GHCup by following [these instructions](https://www.haskell.org/ghcup/install/)
2. Both Cabal and GHC versions can be installed and then set by following [this](https://www.haskell.org/ghcup/guide/). We suggest using Cabal version `3.6.2`, and GHC version `8.6.5`.

-- _Visualising Wasabaye examples_ --

Visualising the provided examples of Wasabaye requires Python3 with the following Python packages:
- `ast`, `matplotlib`, `scipy`, `sklearn`, `numpy`

-- _Benchmarking_ --

To compare benchmarks with MonadBayes and Turing:

- _MonadBayes_ uses the most recent Cabal version `3.6.x`, and requires a GHC version of `>= 8.4.x` and `< 8.10.x`. To execute both Wasabaye and MonadBayes in the same environment, we suggest GHC version `8.6.5`.
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

**Interacting with Wasabaye**

-- _Directly executing Wasabaye_ --

We provide a script below for executing Wasabaye with a set of possible arguments. Alternatively, you can:
  1. Directly execute a Wasabaye program via `cabal run wasabaye <arg>` (corresponding to `Main.hs`), whose output will be written to `model-output.txt`
  2. Visualise this output as a graph via `python3 graph.py <arg>`, which will be saved to `model-output.pdf` .

-- _Executing Wasabaye with the script_ --

The script **`./wasabaye.sh <arg>`** will execute a Wasabaye program and save the output graph to `model-output.pdf` (the results of simulation and inference will of course vary across different executions).

The examples in the paper can be run with one of the following arguments (e.g. `./wasabaye.sh simSIR`), where the prefix `sim` denotes simulation, `lw` denotes Likelihood-Weighting inference, and `mh` denotes Metropolis-Hastings inference:
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

-- _Example models + Creating and executing models_ --

All example models can be found in `src/Examples`, showing variations on how models can be created and executed. [`LogRegr.hs`](https://github.com/min-nguyen/wasabaye/blob/master/src/Examples/LogRegr.hs) documents a particularly representative walk-through. The general process of doing this is as follows:
1. Define an appropriate model of type `Model env es a`
2. Execute a model using one of the library functions `simulate`, `lw`, or `mh` detailed in `src/Inference`; this produces an output in the monad `Sampler`.
3. `Sampler` computations can be evaluated with `sampleIO` (found in `src/Sampler.hs`) to produce an `IO` computation. Examples of this are shown in `Main.hs`.


**Benchmarks**

-- _Directly benchmarking Wasabaye_ --

We provide a script for benchmarking below, but Wasabaye can also be directly benchmarked with `cabal run benchmarking-wasabaye` (corresponding to `benchmarking-wasabaye/Main.hs`). The examples programs used for benchmarking are found in `benchmarking-wasabaye/BenchmarkPrograms.hs`, and they are benchmarked them in `benchmarking-wasabaye/BenchmarkTests.hs`.

-- _Benchmarking with the script_ --

The benchmarks in Appendix A can be reproduced by running the script **`./benchmark.sh <arg>`** with one the following arguments:
- `wasabaye`    : This writes to `benchmarking-wasabaye/wasabaye-benchmarks.csv`
- `monad-bayes` : This writes to `benchmarking-monad-bayes/monad-bayes-benchmarks.csv`
- `turing`      : This writes to `benchmarking-turing/turing-benchmarks.csv`

These will run a given language's benchmarks in the order shown in Appendix A (Figure 8 and then Figure 9), from left to right, and top to bottom.

Each benchmark is given a name of the format `<model>/<algorithm>/<independent-variable>/<value>` where:
- `<model` is the model shown on the LHS Y-axis of the figures
- `<algorithm`> is the model execution algorithm shown on the top of the figures
- `<independent-variable`> is the variable being varied over on the bottom of the figures: this can be `sample-size` which corresponds to Fig 8, or `data-size` corresponding to Fig 9.
- `<value>` is the value assigned to the `<independent-variable>`.

For example, `lda/MH/data-size/80` means a latent-dirichlet model is executed under Metropolis-Hastings for a dataset size of 80.

-- _Creating new Wasabaye benchmarks_ --

We use Criterion to benchmark Wasabaye; a tutorial can be found [here](http://www.serpentine.com/criterion/tutorial.html).

A helper function for benchmarking Wasabaye programs is provided in `benchmarking-wasabaye/BenchmarkTests.hs`,
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

**Paper to artifact overview**

- § 1: Linear regression `(src/Examples/LinRegr.hs)`
  - Simulating  (Fig 1a) is done via `./wasabaye.sh simLinRegr`.
  - Likelihood weighting inference (Fig 1b) is done via `./wasabaye.sh lwLinRegr`.

- § 2: Hidden Markov model `(src/Examples/HMM.hs)`
  - You can find both the loop HMM version (Fig 2) and the modular HMM version (Fig 3) in here.

- § 3: SIR model (`src/Examples/SIR.hs`)
  - § 3.1 The SIR model is the function `hmmSIR'`; the higher-order HMM function it uses is imported from `HMM.hs`, and the `Writer` effect it uses from §5.5 is already integrated.
    - Simulating from this (Fig 4a) can be done via `./wasabaye.sh simSIR`.
    - Metropolis-Hastings inference (Fig 5) is done via `./wasabaye.sh mhSIR`; this takes 1-3 minutes
  - § 3.2.1: The _resusceptible_ extension is defined as `hmmSIRS`, and the _resusceptible + vaccinated_ version is `hmmSIRSV`.

    - Simulating from `hmmSIRS` (Fig 4b) is done via `./wasabaye.sh simSIRS`
    - Simulating from `hmmSIRSV` (Fig 4c) is done via `./wasabaye.sh simSIRSV`.

    Note that their implementations are not as modular as we would like (due to having to redefine the data types `Popl` and `TransParams`) -- the program `SIRModular.hs` shows how one could take steps to resolve this by using extensible records.

- § 4: Embedding
  - § 4.1: The definition of `Prog` is in `src/Prog.hs`, including auxiliary types and functions.
  - § 4.2: The definition of `Model` and the smart constructors for primitive distributions are in `src/Model.hs`.
    The effect type `Dist` (§ 4.2.1) is in `src/Effects/Dist.hs`, and `ObsReader` (§ 4.2.2) is in `src/Effects/ObsReader.hs`.

- § 5: Interpreting multimodal models
  - Intro: Coinflip example: `src/Examples/CoinFlip.hs`
  - § 5.1: Model environments: `src/Env.hs`.
  - § 5.2: Handling `ObsReader`: `src/Effects/ObsReader.hs`.
  - § 5.3: Handling `Dist`: `src/Effects/Dist.hs`.
  - § 5.4: Specialising multimodal models: `src/Model.hs`.
  - § 5.5: Extending models with extra effects: `src/Examples/SIR.hs`

- § 6: Simulation and inference as effect handlers
  - § 6.1: Simulation: `src/Inference/Simulate.hs`. The type of `STrace` is in `src/STrace.hs`.
  - § 6.2.1: Likelihood Weighting: `src/Inference/LW.hs`.
  - § 6.2.2: Metropolis Hastings: `src/Inference.MH.hs`.

---

## QEMU Instructions

The ICFP 2022 Artifact Evaluation Process is using a Debian QEMU image as a
base for artifacts. The Artifact Evaluation Committee (AEC) will verify that
this image works on their own machines before distributing it to authors.
Authors are encouraged to extend the provided image instead of creating their
own. If it is not practical for authors to use the provided image then please
contact the AEC co-chairs before submission.

QEMU is a hosted virtual machine monitor that can emulate a host processor
via dynamic binary translation. On common host platforms QEMU can also use
a host provided virtualization layer, which is faster than dynamic binary
translation.

QEMU homepage: https://www.qemu.org/

### Installation

#### OSX
``brew install qemu``

#### Debian and Ubuntu Linux
``apt-get install qemu-kvm``

On x86 laptops and server machines you may need to enable the
"Intel Virtualization Technology" setting in your BIOS, as some manufacturers
leave this disabled by default. See Debugging.md for details.


#### Arch Linux

``pacman -Sy qemu``

See the [Arch wiki](https://wiki.archlinux.org/title/QEMU) for more info.

See Debugging.md if you have problems logging into the artifact via SSH.


#### Windows 10

Download and install QEMU via the links at

https://www.qemu.org/download/#windows.

Ensure that `qemu-system-x86_64.exe` is in your path.

Start Bar -> Search -> "Windows Features"
          -> enable "Hyper-V" and "Windows Hypervisor Platform".

Restart your computer.

#### Windows 8

See Debugging.md for Windows 8 install instructions.

### Startup

The base artifact provides a `start.sh` script to start the VM on unix-like
systems and `start.bat` for Windows. Running this script will open a graphical
console on the host machine, and create a virtualized network interface.
On Linux you may need to run with `sudo` to start the VM. If the VM does not
start then check `Debugging.md`

Once the VM has started you can login to the guest system from the host.
Whenever you are asked for a password, the answer is `password`. The default
username is `artifact`.

```
$ ssh -p 5555 artifact@localhost
```

You can also copy files to and from the host using scp.

```
$ scp -P 5555 artifact@localhost:somefile .
```

### Shutdown

To shutdown the guest system cleanly, login to it via ssh and use

```
$ sudo shutdown now
```

### Artifact Preparation

Authors should install software dependencies into the VM image as needed,
preferably via the standard Debian package manager. For example, to install
GHC and cabal-install, login to the host and type:

```
$ sudo apt update
$ sudo apt install ghc
$ sudo apt install cabal-install
```

If you really need a GUI then you can install X as follows, but we prefer
console-only artifacts whenever possible.

```
$ sudo apt install xorg
$ sudo apt install xfce4   # or some other window manager
$ startx
```

See Debugging.md for advice on resolving other potential problems.

If your artifact needs lots of memory you may need to increase the value
of the `QEMU_MEM_MB` variable in the `start.sh` script.
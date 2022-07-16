### ProbFX

#### Prelude
ProbFX is a library for probabilistic programming using algebraic effects that implements the paper [**Modular Probabilistic Models via Algebraic Effects**](https://github.com/min-nguyen/prob-fx/blob/master/paper.pdf) -- this provides a comprehensive motivation and walkthrough of this library. To have a more interative and visual play-around with ProbFX, please see https://github.com/min-nguyen/prob-fx: this corresponds parts of the paper to the implementation, and also provides an executable version of ProbFX as a script!

#### Description
ProbFx as a PPL places emphasis on being able to define modular and reusable probabilistic models, where the decision to `sample` or `observe` against a random variable or distribution of a model is delayed until the point of execution; this allows a model to be defined just *once* and then reused for a variety of applications. We also implement a compositional approach towards model execution (inference) by using effect handlers. 

#### Examples and Building Models

We provide and document a large number of example programs [here](https://github.com/min-nguyen/prob-fx/tree/hackage/examples), which show how to define and then executing probabilistic models. In general, the process is:

1. Define an appropriate model of type `Model env es a` and a corresponding model environment of type `Env env`.
2. Execute a model using one of the library functions `simulate`, `lw`, or `mh` detailed in `src/Inference`; this produces an output in the monad `Sampler`.
3. `Sampler` computations can be evaluated with `sampleIO` (found in `src/Sampler.hs`) to produce an `IO` computation. Examples of this are shown in `Main.hs`.

For example:
```haskell 
-- | A linear regression model
linRegr 
  -- | Specify that "y", "m", "c" are observable variables that may later be provided observed values of type @Double@.
  :: Observables env '["y", "m", "c"] Double  
  -- | A model input
  => Double 
  -- | A model that generates an output of type @Double@ 
  -> Model env es Double
linRegr x = do
  -- | Specify model parameter distributions
  m <- normal 0 3 #m      -- Annotating with the observable variable #m lets us later provide observed values for m
  c <- normal 0 5 #c
  σ <- uniform' 1 3       -- One can use primed variants of distributions to disable later providing observed values to that variable
  -- | Specify the distribution of the model output
  y <- normal (m * x + c) σ #y
  return y

-- | Simulating from the linear regression model
simulateLinRegr :: Sampler [(Double, Double)]
simulateLinRegr = do
  let xs  = [0 .. 100]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> nil
  ys_envs <- mapM (SIM.simulate linRegr env) xs
  let ys = map fst ys_envs
  return (zip xs ys)

-- ** (Section 1, Fig 1b) Perform likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
inferLwLinRegr :: Sampler [(Double, Double)]
inferLwLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  lwTrace <- mapM (LW.lw 200 linRegr) xys
  let -- Get output of LW and extract mu samples
      (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  return $ zip mus ps

-- Perform Metropolis-Hastings inference over linear regression
inferMhLinRegr :: Sampler [Double]
inferMhLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> nil]
  mhTrace <- concat <$> mapM (\xy -> MH.mh 100 linRegr xy ["m", "c"]) xys
  let -- Get output of MH and extract mu samples
      mus = concatMap (get #m) mhTrace
  return mus
```

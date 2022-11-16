# ProbFX
## Prelude
ProbFX is a library for probabilistic programming using algebraic effects that implements the work "**Modular Probabilistic Models via Algebraic Effects**" [[**Paper**](https://github.com/min-nguyen/prob-fx/blob/main/paper.pdf) + [**Talk**](https://www.youtube.com/watch?v=xLgqx4DK49k)]. The paper provides a comprehensive motivation and walkthrough of this library. 

To have a more interactive and visual play-around with ProbFX, please see the [**artifact**](https://github.com/min-nguyen/prob-fx/tree/artifact) branch: this corresponds parts of the paper to the implementation, and also provides an executable version of ProbFX as a script.

## Description
ProbFx is a PPL that places emphasis on being able to define modular and reusable probabilistic models, where the decision to `sample` or `observe` against a random variable or distribution of a model is delayed until the point of execution; this allows a model to be defined just *once* and then reused for a variety of applications. We also implement a compositional approach towards model execution (inference) by using effect handlers.

## Building and executing models

A large number of example ProbFX programs are documented in the [**examples**](https://github.com/min-nguyen/prob-fx/tree/main/examples) directory, showing how to define and then execute a probabilistic model.

In general, the process is:

1. Define an appropriate model of type `Model env es a`, and (optionally) a corresponding model environment type `env`.

    For example, a logistic regression model that takes a list of `Double`s as inputs and generates a list of `Bool`s, modelling the probability of an event occurring or not:
    ```haskell
    -- | The model environment type, for readability purposes
    type LogRegrEnv =
      '[  "y" ':= Bool,   -- ^ output
          "m" ':= Double, -- ^ mean
          "b" ':= Double  -- ^ intercept
      ]

    -- | Logistic regression model
    logRegr
      :: (Observable env "y" Bool
       , Observables env '["m", "b"] Double)
      => [Double]
      -> Model env rs [Bool]
    logRegr xs = do
      -- | Specify the distributions of the model parameters
      -- mean
      m     <- normal 0 5 #m
      -- intercept
      b     <- normal 0 1 #b
      -- noise
      sigma <- gamma' 1 1
      -- | Specify distribution of model outputs
      let sigmoid x = 1.0 / (1.0 + exp((-1.0) * x))
      ys    <- foldM (\ys x -> do
                        -- probability of event occurring
                        p <- normal' (m * x + b) sigma
                        -- generate as output whether the event occurs
                        y <- bernoulli (sigmoid p) #y
                        return (ys ++ [y])) [] xs
      return ys
    ```
    The `Observables` constraint says that, for example, `"m"` and `"b"` are observable variables in the model environment `env` that may later be provided a trace of observed values of type `Double`.

    Calling a primitive distribution such as `normal 0 5 #m` lets us later provide observed values for "m" when executing the model.

    Calling a primed variant of primitive distribution such as `gamma' 1 1` will disable observed values from being provided to that distribution.

2. Execute a model under a model environment, using one of the `Inference` library functions.

   Below simulates from a logistic regression model using model parameters `m = 2` and `b = -0.15` but provides no values for `y`: this will result in `m` and `b` being *observed*  but `y` being *sampled*.
    ```haskell
    simulateLogRegr :: Sampler [(Double, Bool)]
    simulateLogRegr = do
      -- | Specify the model inputs
      let xs  = map (/50) [(-50) .. 50]
      -- | Specify the model environment
          env = (#y := []) <:> (#m := [2]) <:> (#b := [-0.15]) <:> nil
      -- | Simulate from logistic regression
      (ys, envs) <- SIM.simulate logRegr env xs
      return (zip xs ys)
    ```

    Below performs Metropolis-Hastings inference on the same model, by providing values for the model output `y` and hence *observing* (conditioning against) them, but providing none for the model parameters `m` and `b` and hence *sampling* them.
    ```haskell
    -- | Metropolis-Hastings inference
    inferMHLogRegr :: Sampler [(Double, Bool)]
    inferMHLogRegr = do
      -- | Simulate data from log regression
      (xs, ys) <- unzip <$> simulateLogRegr
      -- | Specify the model environment
      let env = (#y := ys) <:> (#m := []) <:> (#b := []) <:> nil
      -- | Run MH inference for 20000 iterations
      mhTrace :: [Env LogRegrEnv] <- MH.mh 20000 logRegr (xs, env) ["m", "b"]
      -- | Retrieve values sampled for #m and #b during MH
      let m_samples = concatMap (get #m) mhTrace
          b_samples = concatMap (get #b) mhTrace
      return (zip m_samples b_samples)
    ```
    One may have noticed by now that *lists* of values are always provided to observable variables in a model environment; each run-time occurrence of that variable will then result in the head value being observed and consumed, and running out of values will default to sampling.

    Running the function `mh` returns a trace of output model environments, from which we can retrieve the trace of sampled model parameters via `get #m` and `get #b`. These represent the posterior distribution over `m` and `b`. (The argument `["m", "b"]` to `mh` is optional for indicating interest in learning `#m` and `#b` in particular).

3. `Sampler` computations can be evaluated with `sampleIO :: Sampler a -> IO a` to produce an `IO` computation.

    ```haskell
    sampleIO simulateLogRegr :: IO (Double, Bool)]
    ```

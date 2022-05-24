using Turing
using BenchmarkTools
using DataFrames
using CSV
using Statistics

# Define a simple Normal model with unknown mean and variance.
@model function logRegr(mu, c, σ, xs, ys)
  N = length(xs)
  if ys === missing
      # Initialize `x` if missing
      ys = Vector{Float64}(undef, N)
  end
  mu ~ Normal(0, 3)
  c  ~ Normal(0, 5)
  σ  ~ Uniform(1, 3)
  for n in 1:N
    ys[n] ~  Normal(mu  * xs[n] + c, σ)
  end
  return ys
end

##### Simulation

function logRegrSim(n_datapoints)
  xs = range(0.0, n_datapoints, length=n_datapoints)
  return logRegr(missing, missing, missing, xs, missing)
end

##### Inference

function logRegrInf(n_datapoints)
  simModel = logRegrSim(n_datapoints)
  ys = simModel()
  xs = range(0.0, n_datapoints, length=n_datapoints)
  return logRegr(missing, missing, missing, xs, ys)
end

function parseBenchmark(label, b)
  df = DataFrame(Name = label, Mean = mean(b.times)/(1000000000))
  CSV.write("turing-benchmarks.csv", df, append=true)
end

function benchSimSampleSize()
  print("logRegr/Sim/sample-size/2000" )
  simtraceA = @benchmark sample(logRegrSim(100), Prior(), 2000)
  parseBenchmark("logRegr/Sim/sample-size/2000", simtraceA)

  print("logRegr/Sim/sample-size/4000" )
  simtraceB = @benchmark sample(logRegrSim(100), Prior(), 4000)
  parseBenchmark("logRegr/Sim/sample-size/4000", simtraceB)

  print("logRegr/Sim/sample-size/6000" )
  simtraceC = @benchmark sample(logRegrSim(100), Prior(), 6000)
  parseBenchmark("logRegr/Sim/sample-size/6000", simtraceC)

  print("logRegr/Sim/sample-size/8000" )
  simtraceD = @benchmark sample(logRegrSim(100), Prior(), 8000)
  parseBenchmark("logRegr/Sim/sample-size/8000", simtraceD)

  print("logRegr/Sim/sample-size/10000" )
  simtraceE = @benchmark sample(logRegrSim(100), Prior(), 10000)
  parseBenchmark("logRegr/Sim/sample-size/10000", simtraceE)
end

function benchLWSampleSize()
  print("logRegr/LW/sample-size/2000" )
  lwtrace = @benchmark sample(logRegrInf(100), IS(), 2000)
  parseBenchmark("logRegr/LW/sample-size/2000", lwtrace)

  print("logRegr/LW/sample-size/4000" )
  lwtrace = @benchmark sample(logRegrInf(100), IS(), 4000)
  parseBenchmark("logRegr/LW/sample-size/4000", lwtrace)

  print("logRegr/LW/sample-size/6000" )
  lwtrace = @benchmark sample(logRegrInf(100), IS(), 6000)
  parseBenchmark("logRegr/LW/sample-size/6000", lwtrace)

  print("logRegr/LW/sample-size/8000" )
  lwtrace = @benchmark sample(logRegrInf(100), IS(), 8000)
  parseBenchmark("logRegr/LW/sample-size/8000", lwtrace)

  print("logRegr/LW/sample-size/10000" )
  lwtrace = @benchmark sample(logRegrInf(100), IS(), 10000)
  parseBenchmark("logRegr/LW/sample-size/10000", lwtrace)
end

function benchMHSampleSize()
  print("logRegr/MH/sample-size/2000" )
  mhtrace = @benchmark sample(logRegrInf(100), MH(), 2000)
  parseBenchmark("logRegr/MH/sample-size/2000", mhtrace)

  print("logRegr/MH/sample-size/4000" )
  mhtrace = @benchmark sample(logRegrInf(100), MH(), 4000)
  parseBenchmark("logRegr/MH/sample-size/4000", mhtrace)

  print("logRegr/MH/sample-size/6000" )
  mhtrace = @benchmark sample(logRegrInf(100), MH(), 6000)
  parseBenchmark("logRegr/MH/sample-size/6000", mhtrace)

  print("logRegr/MH/sample-size/8000" )
  mhtrace = @benchmark sample(logRegrInf(100), MH(), 8000)
  parseBenchmark("logRegr/MH/sample-size/8000", mhtrace)

  print("logRegr/MH/sample-size/10000" )
  mhtrace = @benchmark sample(logRegrInf(100), MH(), 10000)
  parseBenchmark("logRegr/MH/sample-size/10000", mhtrace)
end

function benchSimDataSize()
  print("logRegr/Sim/data-size/200" )
  simtraceA = @benchmark sample(logRegrSim(200), Prior(), 2000)
  parseBenchmark("logRegr/Sim/data-size/200", simtraceA)

  print("logRegr/Sim/data-size/400" )
  simtraceB = @benchmark sample(logRegrSim(400), Prior(), 2000)
  parseBenchmark("logRegr/Sim/data-size/400", simtraceB)

  print("logRegr/Sim/data-size/600" )
  simtraceC = @benchmark sample(logRegrSim(600), Prior(), 2000)
  parseBenchmark("logRegr/Sim/data-size/600", simtraceC)

  print("logRegr/Sim/data-size/800" )
  simtraceD = @benchmark sample(logRegrSim(800), Prior(), 2000)
  parseBenchmark("logRegr/Sim/data-size/800", simtraceD)

  print("logRegr/Sim/data-size/1000" )
  simtraceE = @benchmark sample(logRegrSim(1000), Prior(), 2000)
  parseBenchmark("logRegr/Sim/data-size/1000", simtraceE)

end

function benchLWDataSize()
  print("logRegr/LW/data-size/200" )
  lwtrace = @benchmark sample(logRegrInf(200), IS(), 2000)
  parseBenchmark("logRegr/LW/data-size/200", lwtrace)

  print("logRegr/LW/data-size/400" )
  lwtrace = @benchmark sample(logRegrInf(400), IS(), 2000)
  parseBenchmark("logRegr/LW/data-size/400", lwtrace)

  print("logRegr/LW/data-size/600" )
  lwtrace = @benchmark sample(logRegrInf(600), IS(), 2000)
  parseBenchmark("logRegr/LW/data-size/600", lwtrace)

  print("logRegr/LW/data-size/800" )
  lwtrace = @benchmark sample(logRegrInf(800), IS(), 2000)
  parseBenchmark("logRegr/LW/data-size/800", lwtrace)

  print("logRegr/LW/data-size/1000" )
  lwtrace = @benchmark sample(logRegrInf(1000), IS(), 2000)
  parseBenchmark("logRegr/LW/data-size/1000", lwtrace)
end

function benchMHDataSize()
  print("logRegr/MH/data-size/200" )
  mhtrace = @benchmark sample(logRegrInf(200), MH(), 2000)
  parseBenchmark("logRegr/MH/data-size/200", mhtrace)

  print("logRegr/MH/data-size/400" )
  mhtrace = @benchmark sample(logRegrInf(400), MH(), 2000)
  parseBenchmark("logRegr/MH/data-size/400", mhtrace)

  print("logRegr/MH/data-size/600" )
  mhtrace = @benchmark sample(logRegrInf(600), MH(), 2000)
  parseBenchmark("logRegr/MH/data-size/600", mhtrace)

  print("logRegr/MH/data-size/800" )
  mhtrace = @benchmark sample(logRegrInf(800), MH(), 2000)
  parseBenchmark("logRegr/MH/data-size/800", mhtrace)

  print("logRegr/MH/data-size/1000" )
  mhtrace = @benchmark sample(logRegrInf(1000), MH(), 2000)
  parseBenchmark("logRegr/MH/data-size/1000", mhtrace)
end

function main(args)
  if args[1] == "vary-data-size"
    if args[2] == "sim"
      benchSimDataSize()
    elseif args[2] == "lw"
      benchLWDataSize()
    elseif args[2] == "mh"
      benchMHDataSize()
    end
  elseif args[1] == "vary-sample-size"
    if args[2] == "sim"
      benchSimSampleSize()
    elseif args[2] == "lw"
      benchLWSampleSize()
    elseif args[2] == "mh"
      benchMHSampleSize()
    end
  end
end

main(ARGS)
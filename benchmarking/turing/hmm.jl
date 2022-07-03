using Turing
using BenchmarkTools
using DataFrames
using CSV
using Statistics

@model function hmm(trans_p, obs_p, N, xs, ys)
  if ys === missing
      # Initialize `x` if missing
      ys = Vector{Int64}(undef, N)
      ys[1] = 0.0
  end
  if xs === missing
    # Initialize `x` if missing
      xs    = Vector{Int64}(undef, N)
      xs[1] = 0
  end
  trans_p ~ Uniform(0, 1)
  obs_p   ~ Uniform(0, 1)
  # print("trans_p: ", trans_p)
  # print("obs_p: ", obs_p)
  for n in 2:N
    dX    ~ Bernoulli(trans_p)
    # print("dX: ", dX)
    xs[n] = xs[n-1] + dX
    ys[n] ~ Binomial(xs[n], obs_p)
    # print("y: ", ys[n])
  end
  return ys
end

##### Simulation

function hmmSim(n_nodes)
  return hmm(missing, missing, n_nodes, missing, missing)
end

##### Inference

function hmmInf(n_nodes)
  simModel = hmmSim(n_nodes)
  ys = simModel()
  return hmm(missing, missing, n_nodes, missing, ys)
end

function parseBenchmark(label, b)
  df = DataFrame(Name = label, Mean = mean(b.times)/(1000000000))
  CSV.write("turing-benchmarks.csv", df, append=true)
end

function benchSimSampleSize()
  print("hmm/Sim/sample-size/2000" )
  simtrace = @benchmark sample(hmmSim(100), Prior(), 2000)
  parseBenchmark("hmm/Sim/sample-size/2000", simtrace)

  print("hmm/Sim/sample-size/4000" )
  simtrace = @benchmark sample(hmmSim(100), Prior(), 4000)
  parseBenchmark("hmm/Sim/sample-size/4000", simtrace)

  print("hmm/Sim/sample-size/6000" )
  simtrace = @benchmark sample(hmmSim(100), Prior(), 6000)
  parseBenchmark("hmm/Sim/sample-size/6000", simtrace)

  print("hmm/Sim/sample-size/8000" )
  simtrace = @benchmark sample(hmmSim(100), Prior(), 8000)
  parseBenchmark("hmm/Sim/sample-size/8000", simtrace)

  print("hmm/Sim/sample-size/10000" )
  simtrace = @benchmark sample(hmmSim(100), Prior(), 10000)
  parseBenchmark("hmm/Sim/sample-size/10000", simtrace)
end

function benchLWSampleSize()
  print("hmm/LW/sample-size/2000" )
  lwtrace = @benchmark sample(hmmInf(100), IS(), 2000)
  parseBenchmark("hmm/LW/sample-size/2000", lwtrace)

  print("hmm/LW/sample-size/4000" )
  lwtrace = @benchmark sample(hmmInf(100), IS(), 4000)
  parseBenchmark("hmm/LW/sample-size/4000", lwtrace)

  print("hmm/LW/sample-size/6000" )
  lwtrace = @benchmark sample(hmmInf(100), IS(), 6000)
  parseBenchmark("hmm/LW/sample-size/6000", lwtrace)

  print("hmm/LW/sample-size/8000" )
  lwtrace = @benchmark sample(hmmInf(100), IS(), 8000)
  parseBenchmark("hmm/LW/sample-size/8000", lwtrace)

  print("hmm/LW/sample-size/10000" )
  lwtrace = @benchmark sample(hmmInf(100), IS(), 10000)
  parseBenchmark("hmm/LW/sample-size/10000", lwtrace)
end

function benchMHSampleSize()
  print("hmm/MH/sample-size/2000" )
  mhtrace = @benchmark sample(hmmInf(100), MH(), 2000)
  parseBenchmark("hmm/MH/sample-size/2000", mhtrace)

  print("hmm/MH/sample-size/4000" )
  mhtrace = @benchmark sample(hmmInf(100), MH(), 4000)
  parseBenchmark("hmm/MH/sample-size/4000", mhtrace)

  print("hmm/MH/sample-size/6000" )
  mhtrace = @benchmark sample(hmmInf(100), MH(), 6000)
  parseBenchmark("hmm/MH/sample-size/6000", mhtrace)

  print("hmm/MH/sample-size/8000" )
  mhtrace = @benchmark sample(hmmInf(100), MH(), 8000)
  parseBenchmark("hmm/MH/sample-size/8000", mhtrace)

  print("hmm/MH/sample-size/10000" )
  mhtrace = @benchmark sample(hmmInf(100), MH(), 10000)
  parseBenchmark("hmm/MH/sample-size/10000", mhtrace)
end

function benchSimDataSize()
  print("hmm/Sim/data-size/40" )
  simtrace = @benchmark sample(hmmSim(40), Prior(), 2000)
  parseBenchmark("hmm/Sim/data-size/40", simtrace)

  print("hmm/Sim/data-size/80" )
  simtrace = @benchmark sample(hmmSim(80), Prior(), 2000)
  parseBenchmark("hmm/Sim/data-size/80", simtrace)

  print("hmm/Sim/data-size/120" )
  simtrace = @benchmark sample(hmmSim(120), Prior(), 2000)
  parseBenchmark("hmm/Sim/data-size/120", simtrace)

  print("hmm/Sim/data-size/160" )
  simtrace = @benchmark sample(hmmSim(160), Prior(), 2000)
  parseBenchmark("hmm/Sim/data-size/160", simtrace)

  print("hmm/Sim/data-size/200" )
  simtrace = @benchmark sample(hmmSim(200), Prior(), 2000)
  parseBenchmark("hmm/Sim/data-size/200", simtrace)

end

function benchLWDataSize()
  print("hmm/LW/data-size/40" )
  lwtrace = @benchmark sample(hmmInf(40), IS(), 2000)
  parseBenchmark("hmm/LW/data-size/40", lwtrace)

  print("hmm/LW/data-size/80" )
  lwtrace = @benchmark sample(hmmInf(80), IS(), 2000)
  parseBenchmark("hmm/LW/data-size/80", lwtrace)

  print("hmm/LW/data-size/120" )
  lwtrace = @benchmark sample(hmmInf(120), IS(), 2000)
  parseBenchmark("hmm/LW/data-size/120", lwtrace)

  print("hmm/LW/data-size/160" )
  lwtrace = @benchmark sample(hmmInf(160), IS(), 2000)
  parseBenchmark("hmm/LW/data-size/160", lwtrace)

  print("hmm/LW/data-size/200" )
  lwtrace = @benchmark sample(hmmInf(200), IS(), 2000)
  parseBenchmark("hmm/LW/data-size/200", lwtrace)

end

function benchMHDataSize()
  print("hmm/MH/data-size/40" )
  mhtrace = @benchmark sample(hmmInf(40), MH(), 2000)
  parseBenchmark("hmm/MH/data-size/40", mhtrace)

  print("hmm/MH/data-size/80" )
  mhtrace = @benchmark sample(hmmInf(80), MH(), 2000)
  parseBenchmark("hmm/MH/data-size/80", mhtrace)

  print("hmm/MH/data-size/120" )
  mhtrace = @benchmark sample(hmmInf(120), MH(), 2000)
  parseBenchmark("hmm/MH/data-size/120", mhtrace)

  print("hmm/MH/data-size/160" )
  mhtrace = @benchmark sample(hmmInf(160), MH(), 2000)
  parseBenchmark("hmm/MH/data-size/160", mhtrace)

  print("hmm/MH/data-size/200" )
  mhtrace = @benchmark sample(hmmInf(200), MH(), 2000)
  parseBenchmark("hmm/MH/data-size/200", mhtrace)
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
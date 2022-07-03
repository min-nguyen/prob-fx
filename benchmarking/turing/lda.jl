using Turing
using BenchmarkTools
using DataFrames
using CSV
using Statistics

function wordsToIdxs(vocab, words)
  n_words = length(words)
  word_idxs = Vector{Int64}(undef, n_words)
  for i in 1:n_words
    word_idxs[i] = findfirst(w -> w==words[i], vocab)
  end
  return word_idxs
end

function idxsToWords(vocab, word_idxs)
  n_words = length(word_idxs)
  words = Vector{String}(undef, n_words)
  for i in 1:n_words
    words[i] = vocab[word_idxs[i]]
  end
  return word_idxs
end

@model function topicModel(doc_topic_ps, topic_word_ps, vocab, n_topics, n_words, word_idxs)
  # simulation
  if word_idxs === missing
    # initialise list of words observed
    words     = Vector{String}(undef, n_words)
    # initialise list of corresponding word indexes observed
    word_idxs = Vector{Int64}(undef, n_words)
  # inference
  else
    #  set length of words
    n_words   = length(word_idxs)
    # initialise list of words observed
    words     = Vector{String}(undef, n_words)
    # set list of words observed
    for i in 1:n_words
      words[i] = vocab[word_idxs[i]]
    end
  end

  # print(word_idxs)
  if topic_word_ps === missing
    # initialise list of word probabilities for each topic
    topic_word_ps = Vector{Vector{Float64}}(undef, n_topics)
  end

  # set list of topic probabilities for the document
  doc_topic_ps ~ Dirichlet(ones(n_topics))
  for i in 1:n_topics
    # set list of word probabilities for each topic
    topic_word_ps[i] ~ Dirichlet((ones(length(vocab))))
  end

  # initialise list of topics observed
  topic_obs = Vector{Int64}(undef, n_words)

  for i in 1:n_words
    # observe a topic
    topic_obs[i] ~ Categorical(doc_topic_ps)
    # fetch the topic's corresponding word distribution
    word_ps = topic_word_ps[topic_obs[i]]
    # observe a word index for that topic
    word_idxs[i] ~ Categorical(word_ps)
  end
  # print(word_idxs)

  # print(word_idxs)
  # print(doc_topic_ps)
  # print(topic_word_ps)
  return word_idxs
end

vocab     = ["DNA", "evolution", "parsing", "phonology"]
words     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]
word_idxs = wordsToIdxs(vocab, words)

##### Simulation

function topicSim(n_words)
  return topicModel(missing, missing, vocab, 2, n_words, missing)
end



##### Inference
function topicInfer(n_words)
  simModel = topicSim(n_words)
  ys = simModel()
  return topicModel(missing, missing, vocab, 2, n_words, ys)
end

function parseBenchmark(label, b)
  df = DataFrame(Name = label, Mean = mean(b.times)/(1000000000))
  CSV.write("turing-benchmarks.csv", df, append=true)
end

function benchSimSampleSize()
  print("lda/Sim/sample-size/2000" )
  simtrace = @benchmark sample(topicSim(100), Prior(), 2000)
  parseBenchmark("lda/Sim/sample-size/2000", simtrace)

  print("lda/Sim/sample-size/4000" )
  simtrace = @benchmark sample(topicSim(100), Prior(), 4000)
  parseBenchmark("lda/Sim/sample-size/4000", simtrace)

  print("lda/Sim/sample-size/6000" )
  simtrace = @benchmark sample(topicSim(100), Prior(), 6000)
  parseBenchmark("lda/Sim/sample-size/6000", simtrace)

  print("lda/Sim/sample-size/8000" )
  simtrace = @benchmark sample(topicSim(100), Prior(), 8000)
  parseBenchmark("lda/Sim/sample-size/8000", simtrace)

  print("lda/Sim/sample-size/10000" )
  simtrace = @benchmark sample(topicSim(100), Prior(), 10000)
  parseBenchmark("lda/Sim/sample-size/10000", simtrace)
end

function benchLWSampleSize()
  print("lda/LW/sample-size/2000" )
  lwtrace = @benchmark sample(topicInfer(100), IS(), 2000)
  parseBenchmark("lda/LW/sample-size/2000", lwtrace)

  print("lda/LW/sample-size/4000" )
  lwtrace = @benchmark sample(topicInfer(100), IS(), 4000)
  parseBenchmark("lda/LW/sample-size/4000", lwtrace)

  print("lda/LW/sample-size/6000" )
  lwtrace = @benchmark sample(topicInfer(100), IS(), 6000)
  parseBenchmark("lda/LW/sample-size/6000", lwtrace)

  print("lda/LW/sample-size/8000" )
  lwtrace = @benchmark sample(topicInfer(100), IS(), 8000)
  parseBenchmark("lda/LW/sample-size/8000", lwtrace)

  print("lda/LW/sample-size/10000" )
  lwtrace = @benchmark sample(topicInfer(100), IS(), 10000)
  parseBenchmark("lda/LW/sample-size/10000", lwtrace)
end

function benchMHSampleSize()
  print("lda/MH/sample-size/2000" )
  mhtrace = @benchmark sample(topicInfer(100), MH(), 2000)
  parseBenchmark("lda/MH/sample-size/2000", mhtrace)

  print("lda/MH/sample-size/4000" )
  mhtrace = @benchmark sample(topicInfer(100), MH(), 4000)
  parseBenchmark("lda/MH/sample-size/4000", mhtrace)

  print("lda/MH/sample-size/6000" )
  mhtrace = @benchmark sample(topicInfer(100), MH(), 6000)
  parseBenchmark("lda/MH/sample-size/6000", mhtrace)

  print("lda/MH/sample-size/8000" )
  mhtrace = @benchmark sample(topicInfer(100), MH(), 8000)
  parseBenchmark("lda/MH/sample-size/8000", mhtrace)

  print("lda/MH/sample-size/10000" )
  mhtrace = @benchmark sample(topicInfer(100), MH(), 10000)
  parseBenchmark("lda/MH/sample-size/10000", mhtrace)
end

function benchSimDataSize()
  print("lda/Sim/data-size/40" )
  simtrace = @benchmark sample(topicSim(40), Prior(), 2000)
  parseBenchmark("lda/Sim/data-size/40", simtrace)

  print("lda/Sim/data-size/80" )
  simtrace = @benchmark sample(topicSim(80), Prior(), 2000)
  parseBenchmark("lda/Sim/data-size/80", simtrace)

  print("lda/Sim/data-size/120" )
  simtrace = @benchmark sample(topicSim(120), Prior(), 2000)
  parseBenchmark("lda/Sim/data-size/120", simtrace)

  print("lda/Sim/data-size/160" )
  simtrace = @benchmark sample(topicSim(160), Prior(), 2000)
  parseBenchmark("lda/Sim/data-size/160", simtrace)

  print("lda/Sim/data-size/200" )
  simtrace = @benchmark sample(topicSim(200), Prior(), 2000)
  parseBenchmark("lda/Sim/data-size/200", simtrace)

end

function benchLWDataSize()
  print("lda/LW/data-size/40" )
  lwtrace = @benchmark sample(topicInfer(40), IS(), 2000)
  parseBenchmark("lda/LW/data-size/40", lwtrace)

  print("lda/LW/data-size/80" )
  lwtrace = @benchmark sample(topicInfer(80), IS(), 2000)
  parseBenchmark("lda/LW/data-size/80", lwtrace)

  print("lda/LW/data-size/120" )
  lwtrace = @benchmark sample(topicInfer(120), IS(), 2000)
  parseBenchmark("lda/LW/data-size/120", lwtrace)

  print("lda/LW/data-size/160" )
  lwtrace = @benchmark sample(topicInfer(160), IS(), 2000)
  parseBenchmark("lda/LW/data-size/160", lwtrace)

  print("lda/LW/data-size/200" )
  lwtrace = @benchmark sample(topicInfer(200), IS(), 2000)
  parseBenchmark("lda/LW/data-size/200", lwtrace)
end

function benchMHDataSize()
  print("lda/MH/data-size/40" )
  mhtrace = @benchmark sample(topicInfer(40), MH(), 2000)
  parseBenchmark("lda/MH/data-size/40", mhtrace)

  print("lda/MH/data-size/80" )
  mhtrace = @benchmark sample(topicInfer(80), MH(), 2000)
  parseBenchmark("lda/MH/data-size/80", mhtrace)

  print("lda/MH/data-size/120" )
  mhtrace = @benchmark sample(topicInfer(120), MH(), 2000)
  parseBenchmark("lda/MH/data-size/120", mhtrace)

  print("lda/MH/data-size/160" )
  mhtrace = @benchmark sample(topicInfer(160), MH(), 2000)
  parseBenchmark("lda/MH/data-size/160", mhtrace)

  print("lda/MH/data-size/200" )
  mhtrace = @benchmark sample(topicInfer(200), MH(), 2000)
  parseBenchmark("lda/MH/data-size/200", mhtrace)
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
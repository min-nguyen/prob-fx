#!/bin/bash
if  [[ $# -eq 0 ]] || [ "$1" == "-h" ] || [ "$1" == "--help" ];
  then
    echo "Usage: ./`basename $0` <arg>"
    echo "Arg options: [prob-fx, monad-bayes, turing]"
    exit
fi
if [ $1 == "monad-bayes" ]
then cd benchmarking/monad-bayes && cabal run
elif  [ $1 == "prob-fx" ]
then cabal run benchmark
elif  [ $1 == "turing" ];
then cd benchmarking/turing;
     echo "Running LinRegr/Simulation for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia linregr.jl "vary-sample-size" "sim";
     echo "Running LinRegr/LW for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia linregr.jl "vary-sample-size" "lw";
     echo "Running LinRegr/MH for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia linregr.jl "vary-sample-size" "mh";

     echo "Running HMM/Simulation for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia hmm.jl "vary-sample-size" "sim";
     echo "Running HMM/LW for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia hmm.jl "vary-sample-size" "lw";
     echo "Running HMM/MH for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia hmm.jl "vary-sample-size" "mh";

     echo "Running LDA/Simulation for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia lda.jl "vary-sample-size" "sim";
     echo "Running LDA/LW for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia lda.jl "vary-sample-size" "lw";
     echo "Running LDA/MH for sample sizes: 2000, 4000, 6000, 8000, 10000";
     julia lda.jl "vary-sample-size" "mh";

     echo "Running LinRegr/Simulation for data sizes: 200, 400, 600, 800, 1000";
     julia linregr.jl "vary-data-size" "sim";
     echo "Running LinRegr/LW for data sizes: 200, 400, 600, 800, 1000";
     julia linregr.jl "vary-data-size" "lw";
     echo "Running LinRegr/MH for data sizes: 200, 400, 600, 800, 1000";
     julia linregr.jl "vary-data-size" "mh";

     echo "Running HMM/Simulation for data sizes: 40, 80, 120, 160, 200";
     julia hmm.jl "vary-data-size" "sim";
     echo "Running HMM/LW  for data sizes: 40, 80, 120, 160, 200";
     julia hmm.jl "vary-data-size" "lw";
     echo "Running HMM/MH  for data sizes: 40, 80, 120, 160, 200";
     julia hmm.jl "vary-data-size" "mh";

     echo "Running LDA/Simulation  for data sizes: 40, 80, 120, 160, 200";
     julia lda.jl "vary-data-size" "sim";
     echo "Running LDA/LW for data sizes: 40, 80, 120, 160, 200";
     julia lda.jl "vary-data-size" "lw";
     echo "Running LDA/MH for data sizes: 40, 80, 120, 160, 200";
     julia lda.jl "vary-data-size" "mh";
     exit
else
  echo "Argument '$1' unrecognized. "
  echo "Usage: ./`basename $0` <arg>"
  echo "Arg options: [prob-fx, monad-bayes, turing]"
  exit
fi

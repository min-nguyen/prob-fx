#!/bin/bash
function exists_in_list() {
    LIST=$1
    DELIMITER=$2
    VALUE=$3
    [[ "$LIST" =~ ($DELIMITER|^)$VALUE($DELIMITER|$) ]]
}

possible_arguments="simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon, mhSchool"

if [[ $# -eq 0 ]] || [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
  echo "Usage: ./`basename $0` <arg>"
  echo "Arg options: [$possible_arguments]" 
elif exists_in_list "$possible_arguments" ", " $1; then
  cabal run prob-fx $1
  python3 graph.py $1
else
  echo "Argument '$1' unrecognized. "
  echo "Usage: ./`basename $0` <arg>"
  echo "Arg options: [$possible_arguments]"
  exit
fi


#!/bin/bash

DIR=.

# parse options
while getopts 'ACd:e:Ef:hHiILm:MNn:o:O:p:Pq:rsStT:vVw:' OPTION
do
case $OPTION  in
  d)
   DIR="$OPTARG"
   ;;
esac
done
shift $(($OPTIND-1))

case=$1
infile=$DIR/${case%.*}.fds
outfile=$DIR/${case%.*}.out
if [  -e $outfile ]; then
  if [[ `grep -rI 'completed successfully' $outfile` == "" ]] && [[  `grep -rI 'Set-up only' $outfile` == "" ]]; then
    echo "ERROR: the case $infile started but did not complete"
  fi
else
  echo "ERROR: the case $infile did not run ($outfile does not exist)"
fi

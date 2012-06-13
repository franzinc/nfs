#!/bin/bash

## Performance testing of the nfs server
set -e

DIR_LOG="/tmp"

relative_path=`dirname $0`
absolute_path=`cd $relative_path;pwd`


HAMMER="$absolute_path/../hammernfs"
HAMMER_LOG=${3:-`mktemp --tmpdir=$DIR_LOG nfs-performance.XXXXXX`}
HAMMER_PATH=${2:-"win2k8-64.win.franz.com:/c/hammer"}
ITERATIONS=${1:-"5"}

## TODO add some cli args and a help menu.

echo "Running $ITERATIONS iterations of nfs performance tests and logging to: $HAMMER_LOG"
for i in `seq 1 $ITERATIONS`
do
  for ver in 2
  do
    for duration in 300
    do
      for blocksize in 8192
      do
        for proto in udp
        do
	  echo "$HAMMER -v $ver -5 $duration -b $blocksize -p $proto $HAMMER_PATH" >> $HAMMER_LOG
	  $HAMMER -v $ver -t $duration -b $blocksize -p $proto $HAMMER_PATH >> $HAMMER_LOG
	done
      done
    done
  done
done

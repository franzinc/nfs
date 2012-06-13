#!/bin/bash

set -e

DIR_LOG="/tmp"

relative_path=`dirname $0`
absolute_path=`cd $relative_path;pwd`

HAMMER="$absolute_path/../hammernfs"
HAMMER_LOG=${2:-`mktemp --tmpdir=$DIR_LOG nfs-correctness.XXXXXXX`}
#trap "rm $HAMMER_LOG" EXIT
HAMMER_PATH=${1:-"win2k8-64.win.franz.com:/c/hammer"}

## TODO add some cli args and a help menu.

echo "Correctness testing hammer nfs and logging to: $HAMMER_LOG"
for ver in 2 3
do
  for duration in 60 90 120 600 3600
  do
    for blocksize in 1024 2048 4096 8192
    do
      for proto in udp tcp
      do
        echo "$HAMMER -v $ver -t $duration -b $blocksize -p $proto $HAMMER_PATH" >> $HAMMER_LOG
        $HAMMER -v $ver -t $duration -b $blocksize -p $proto $HAMMER_PATH >> $HAMMER_LOG
      done
    done
  done
done

trap "" EXIT

exit 0

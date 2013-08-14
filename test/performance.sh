#!/bin/bash
# Performance testing of the nfs server
# As a side effect it does do some correctness testing.

set -eu

make hammernfs

hammernfs="./hammernfs"
# The log file is lisp readable
logfile="test/performance.log"
nfstestpath="hobart256:/nfs.test/nfstestfile"

iterations=${1-5}
duration=${2-60}

cat <<EOF
Doing $iterations iterations, logging to: $logfile

EOF

host=$(hostname -s)

function logit {
    echo "$@" >> $logfile
}

cp /dev/null $logfile
logit ';;;' starting performance tests on $host $(date)

function hammertime {
    logit ';;' $hammernfs "$@"
    echo ';;' $hammernfs "$@"
    $hammernfs "$@" >> $logfile
}

# The number of combinations is 5x2x4x2 = 80, and 1m per iteration is
# 80 minutes to run through this script.

for ver in 2 3; do
    for bs in 512 2048 4096 8192; do
	for transport in tcp udp; do
	    for i in $(seq 1 $iterations); do
		hammertime -i $i -v $ver -t $duration -b $bs \
		    -p $transport $nfstestpath
	    done
	done
    done
done

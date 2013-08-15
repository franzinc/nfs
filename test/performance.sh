#! /bin/bash
# Performance testing of the nfs server
# As a side effect it does do some correctness testing.

set -eu

if [ -d c:/ ]; then exe=.exe; else exe=; fi

make hammernfs$exe

hammernfs="./hammernfs$exe"
# The log file is lisp readable
logfile="${1-test/performance.log}"
# Use localhost here, to maximize consistency.  It seems to help
# a little.
nfstestpath="127.0.0.1:/nfs.test/nfstestfile"

# The number of combinations is 5x2x4x2 = 80, and at 60s per iteration that
# is 80 minutes to run through this script.
iterations=5
duration=60

echo Logging to: $logfile

host=$(hostname)

function logit {
    echo "$@" >> $logfile
}

cp /dev/null $logfile
logit ';;;' performance tests, $host - $(date)

function hammertime {
    logit ';;' $hammernfs "$@"
    echo $hammernfs "$@"
    $hammernfs "$@" >> $logfile
}

for ver in 2 3; do
    for bs in 512 2048 4096 8192; do
	for transport in tcp udp; do
	    logit '('
	    for i in $(seq 1 $iterations); do
		hammertime -i $i -v $ver -t $duration -b $bs \
		    -p $transport $nfstestpath
	    done
	    logit ')'
	done
    done
done

# So we can commit
cvt -f -d $logfile

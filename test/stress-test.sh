#!/bin/bash
# stress test the server
# usage: $0 [number-of-iterations]
# one iteration takes about 1m on freon/thor

set -eu

nfsdir=/net/thor/nfs.test
localdir=/home/tmp/layer/nfs.test

function makedata {
# make 100mb of data in $1
    local n
    mkdir -p $1
    for n in $(seq 1 10); do
	# would be nice if dd had a -q flag
	echo making $1/$n
	dd if=/dev/urandom of=$1/file${n}.10m \
	    bs=1M count=10 &> /dev/null
	echo this is a small file > $1/file${n}.small
    done
}

localroot=$localdir/stress.test
nfsroot=$nfsdir/stress.test

function cleanup {
    echo cleanup
    rm -fr $nfsroot
    rm -fr $localroot
    mkdir $nfsroot
    mkdir $localroot
}

function copy {
    echo copy to $2
    cp -rp $1 $2
}

for i in $(seq 1 ${1-1}); do
    echo ==================== iteration $i - $(date)
    cleanup
    makedata $localroot/dir
    copy $localroot/dir ${nfsroot}/dir
    prev=
    for j in $(seq 1 10); do
	copy $localroot/dir${prev} ${nfsroot}/dir${j}
	copy ${nfsroot}/dir${j} ${localroot}/dir${j}
	prev=$j
    done
    copy ${nfsroot}/dir${j} ${localroot}/dir${j}
    echo comparing results
    if diff $localroot $nfsroot > /dev/null; then
	echo OK
    else
	echo $localroot and $nfsroot are different
	exit 1
    fi
done

cleanup

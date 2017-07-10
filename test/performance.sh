#! /bin/bash
# Performance testing of the nfs server

set -eu

iterations=5
duration=60

# time to run this script, for usage text:
minutes=$(( iterations * 2 * 4 * 2 * duration / 60 ))

function usage {
    [ "${*-}" ] && echo "Error: $*" 1>&2
    cat 1>&2 <<EOF
Usage: $(basename $0) nfstestpath results-file

nfstestpath is the path to test.  results-file must not exist.

results-file is the file to which results are logged, in the
results/ directory.

The time to complete test is $minutes minutes.

Example:
 \$ test/performance.sh 192.132.95.228:/nfs.test/nfstestfile 6.3.0.rc1

would create results/6.3.0.rc1 with the raw performance data.
EOF
    exit 1
}

function errordie {
    [ "${*-}" ] && echo "Error: $*" 1>&2
    exit 1
}

# hammernfs fails randomly on Cygwin
[ -d c:/ ] && errordie This script is unreliable when run on Windows

while [ $# -gt 0 ]; do
    case $1 in
	--help) usage ;;
	-*) usage bad argument: $1 ;;
	*)  [ $# -eq 2 ] || usage wrong number of arguments
	    break
	    ;;
    esac
    shift
done

if [[ $1 =~ : ]]; then
    :
else
    errordie nfstestpath should have the form host:path
fi

nfstestpath=$1
logfile=results/$2

[ -f "$logfile" ] && errordie $logfile exists

make hammernfs
hammernfs="./hammernfs"

[ "$logfile" ] && echo Logging to: $logfile

host=$(hostname)

function logit {
    if [ "$logfile" ]; then
	echo "$@" >> $logfile
    else
	echo "$@"
    fi
}

[ "$logfile" ] && cp /dev/null $logfile
logit ';;;' performance tests, client host is: $host, $(date)

function hammertime {
    logit ';;' $hammernfs "$@"
    echo $hammernfs "$@"
    if [ "$logfile" ]; then
	$hammernfs "$@" >> $logfile
    else
	$hammernfs "$@"
    fi
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

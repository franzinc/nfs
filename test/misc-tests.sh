#! /usr/bin/env bash

set -eu

function die {
    [ "${*-}" ] && echo "Error: $*" 1>&2
    exit 1
}

testhost=$1
nfspath=$2
hostpath=$3

# Make sure we can ls the entire c:/ directory
function test1 {
    local count1a="$(ssh $testhost /bin/ls -1 $hostpath | wc -l)"
    local count1b="$(/bin/ls -1 $nfspath | wc -l)"
    local count2a="$(ssh $testhost /bin/ls -l $hostpath | tail -n +2 | wc -l)"
    local count2b="$(/bin/ls -l $nfspath | tail -n +2 | wc -l)"

    [ "$count1a" ] || die count1a is empty
    [ "$count1b" ] || die count1b is empty
    [ "$count2a" ] || die count2a is empty
    [ "$count2b" ] || die count2b is empty

    [ $count1a -eq $count1b ] || die count1 differs
    [ $count2a -eq $count2b ] || die count2 differs
    [ $count1a -eq $count2a ] || die count1/2 differs: $count1a $count2a
}

test1

echo SUCCESS

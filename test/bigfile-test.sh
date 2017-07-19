#!/bin/bash
# Test reads and writes over 4GB

set -eu

localdir=$1
nfsdir=$2

tempfile="tempfile.$$"

trap "rm -f $localdir/$tempfile $nfsdir/$tempfile" EXIT

echo "create remote file over 4Gig..."
dd if=/dev/zero of=$nfsdir/$tempfile   bs=1M count=5000
echo "create local file over 4Gig..."
dd if=/dev/zero of=$localdir/$tempfile bs=1M count=5000

echo "compare files..."
if ! cmp $localdir/$tempfile $nfsdir/$tempfile; then
    echo ERROR: big file of zeros differ
    exit 1
fi

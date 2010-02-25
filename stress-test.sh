#!/bin/bash

set -e

# stress test the nfs server.

remotedir=$1
localdir=$2

if [ ! -d "$localdir" ]
then
  mkdir $localdir
else
  echo "Unable to create localdir, $localdir"
  exit 1
fi

trap "if [ -d $localdir ]; then rmdir $localdir; fi" EXIT

echo "Mounting $remotedir on $localdir"
su -c "mount $remotedir $localdir" -

trap "if [ -d $localdir ]; then su -c \"umount $localdir\" -; rmdir $localdir; fi" EXIT

tempfile="tempfile.$$"

echo "creating a file over 4Gig's called $localdir/$tempfile"
dd if=/dev/zero of=$localdir/$tempfile bs=1M count=5000

trap "if [ -d $localdir ]; then rm $localdir/$tempfile; su -c \"umount $localdir\" -; rmdir $localdir; fi" EXIT

exit 0

#! /bin/sh -e

file=setup.log.full
export=/export
time=20
# removed 1024
block_sizes="4096 8192"

for bs in $block_sizes; do
	cmd="./hammernfs -q -t $time -b $bs -u 483 -g 50 -e $export -f $file"
	echo ====================================================================
	echo $cmd
	$cmd
done 

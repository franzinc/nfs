#! /bin/sh

if test -f $1/CVS/Tag; then
	echo -r `cat $1/CVS/Tag | sed 's/^T//'`;
else
	echo -A
fi

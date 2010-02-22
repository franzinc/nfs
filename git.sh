#! /bin/sh
#
# Copies of this script in the lfs and www2-data repos

dirs=". date demoware"

set -eu

module_current_branch()
{
# $1 == the directory to determine the branch of.

    if test -f $1/.git/HEAD; then
	cat $1/.git/HEAD | sed -e 's,ref: refs/heads/,,'
#also:
#       (cd $1; git branch | awk '/^* / { print $2; }')
    fi
}

back=$(pwd)
for d in $dirs; do
    branch=`module_current_branch $d`
    echo ======= Directory $d "(branch: $branch)"
    if test -n "$*"; then
	cd $d
	if test "$1" = "status"; then
	    git "$@" || true
	elif git "$@"; then
	    : # OK
	else
	    echo Command failed with status $?
	fi
	cd $back
    fi
done

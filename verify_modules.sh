#! /bin/sh

switch_to_branch()
{
# $1 = target branch

    if (git branch | grep ' '$1'$') > /dev/null; then
	echo "    already exists as a local branch"
	git checkout -q $1
    else
	git checkout -q -b $1 origin/$1
    fi
}

back=$(pwd)
for thing in $*; do
    if test "$thing" = "."; then
	branch_check_only=xxx
    else
	branch_check_only=
    fi

    branch=`echo $thing | sed 's/\(.*\):\(.*\)/\2/'`
    if test -z "$branch_check_only"; then
	repo=`echo $thing | sed 's/\(.*\):\(.*\)/\1/'`
	if test ! -d $repo; then
	    echo "checking out $repo..."
	    git clone -q git:/repo/git/$repo $repo
	fi
    fi

    cd $repo

    current="`git branch | awk '/^* / { print $2; }'`"
    if test "$current" != "$branch"; then
	echo "moving onto $branch branch..."
	switch_to_branch $branch
    fi

    cd $back
done

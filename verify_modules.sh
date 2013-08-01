#! /bin/sh

switch_to_branch()
{
# $1 = remote (e.g., origin)
# $2 = target branch (e.g., acl82)
    local remote=$1
    local branch=${2-}
    git fetch $remote
    if test -n "${branch}"; then
	if git_branch_exists_p $branch; then
	    # already exists as a local branch, get on it:
	    git checkout -q $branch
	elif git_remote_exists_p $remote/$branch; then
	    # no remote tracking branch, create it
	    git checkout -q -b $branch $remote/$branch
	else
	    # does not exist
	    echo ERROR: $remote/$branch does not exist.
	    exit 1
	fi
    fi
}

git_branch_exists_p()
{
    git show-ref --quiet --verify -- "refs/heads/$1"
}

git_remote_exists_p()
{
    git show-ref --quiet --verify -- "refs/remotes/$1"
}

compute_origin_base_url()
{
    dirname `git config --get remote.origin.url`
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
	    url=`compute_origin_base_url`/$repo
	    git clone -q $url $repo
	fi
    fi

    cd $repo

    current="`git branch | awk '/^* / { print $2; }'`"
    if test "$current" != "$branch"; then
	echo "moving $repo onto $branch branch..."
	switch_to_branch origin $branch
    fi

    cd $back
done

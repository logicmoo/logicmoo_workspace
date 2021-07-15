#!/bin/sh
branch="$1"
ref="$(git show-ref "refs/heads/$branch")"
if [ -z "$ref" ]; then
    branch="master"
fi
git checkout "$branch"
git pull origin "$branch"
git commit -am "$branch"
git push 


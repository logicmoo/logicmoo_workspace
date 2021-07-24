#!/bin/bash

# This script, executed at the root of a git repository, deletes traces of every old file in this repository, index + blob on all branches
# It can take 10-30 minutes to run and will print regular warning stating than some references are unchanged
# time ./clear_git_repositor.bash >cleaning.log 

# We need several passes to clean files renamed multiple times (git log --find-renames prevents its deletion for each renaming)
# MAXIMUM_PASSES should be more than the maximum number of renamings/movings for any file, if not then we might keep some traces of former files
MAXIMUM_PASSES=10  # Maximum number of passes

# We pass files to filter-branch in groups of NUMBER_GROUPS files in order not to exceed the maximum number of arguments
NUMBER_GROUPS=200  # Number of files in groups

IFS=$'\n' # Specifying the Internal Field Separator to iterate over file names containing whitespaces
INITIAL_SIZE=`du -hs .`

filter_branch() 
{ 
    FILES="$@"
    echo "Deleting all traces of $FILES ..."
    git filter-branch --tag-name-filter cat --index-filter "git rm -r --cached --ignore-unmatch -- $FILES" --prune-empty -f -- --all
}

for PASS in `seq 1 $MAXIMUM_PASSES`;
do
    echo "###################### PASS $PASS"
    DELETED=`git log --find-renames --all --pretty=format: --name-only --diff-filter=D | sort -u| grep -v '^$'`
    if [ -n "$DELETED" ];
    then
        # Make group of files to rewrite to prevent "Argument list too long" errors on big repos
        GROUP=""
        FILE_NUM=0
        for FILE in $DELETED;
        do
            if [ $FILE_NUM -le $NUMBER_GROUPS ];
            then
                GROUP=$GROUP"'$FILE' "
                ((FILE_NUM++))
            else
                filter_branch $GROUP
                GROUP=""
                FILE_NUM=0
            fi
        done
        filter_branch $GROUP  # Process the rest of the files insufficient to form a group
    else
        echo "No more deleted files found after $PASS passes, exiting"
        break
    fi
done
unset $IFS

echo "Deleting empty commits..."  # Some empty merging commits might be erased now
git filter-branch -f --prune-empty --tag-name-filter cat -- --all

# Prune all references with garbage collection and reclaim space
echo "Pruning commits"
rm -rf .git/refs/original/
git reflog expire --expire=now --all
git gc --aggressive --prune=now

FINAL_SIZE=`du -hs .`
echo "Cleaning ended, initial size: $INITIAL_SIZE final size: $FINAL_SIZE"
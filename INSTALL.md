#!/bin/bash

echo "#* "
cd $LOGICMOO_WS

#git submodule init
#git submodule update
#git submodule sync --recursive
git fetch --recurse-submodules
git status -v --show-stash

( ./INSTALL-DEPS.md )
( ./INSTALL-SWI.md )




#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

./logicmoo_env.sh

#d $LOGICMOO_WS

#git submodule init
#git submodule update
#git submodule sync --recursive
git fetch --recurse-submodules
git status -v --show-stash

(source ./INSTALL-DEPS.md )
(source ./INSTALL-SWI.md )




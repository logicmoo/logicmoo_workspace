#!/bin/bash

( ./INSTALL-DEPS.md )

./logicmoo_env.sh

(
cd $LOGICMOO_WS

#mkdir -p bin/
#mkdir .local/share/swi-prolog/pack -p
#chmod 555 .local/share/swi-prolog/pack

git clone https://github.com/SWI-Prolog/swipl-devel.git swipl-devel

(cd swipl-devel/
git reset --hard HEAD
git clean -f -x 
git checkout origin/master . -f
git submodule update --init
git pull --recurse-submodules
patch -p1 --merge < ../dmiles-attvar.patch
git status)

(cd swipl-devel
 mkdir -p build
 cd build
 cmake -DCMAKE_INSTALL_PREFIX=$LOGICMOO_WS -G "Unix Makefiles" ..
 make
 ctest -j 8
 make install)

)
stty sane


#!/bin/bash

./logicmoo_env.sh .

DIR="$LOGICMOO_WS/lib/swipl"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: SWI-Prolog is hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
    return 0 2>/dev/null
    exit 0


fi

echo "#* "
echo "#* Install deps..."
echo "#* "

( cd $LOGICMOO_WS
  ./INSTALL-DEPS.md )

(
cd $LOGICMOO_WS

echo "#* "
echo "#* Installing swipl in ${DIR}..."
echo "#* "

#mkdir -p bin/
#mkdir .local/share/swi-prolog/pack -p
#chmod 555 .local/share/swi-prolog/pack
find packs_* -name "*.qlf" -delete

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



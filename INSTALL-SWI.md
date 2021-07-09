#!/bin/bash -x

function MAINTAINER {
 echo $*
}
function RUN {
 $*
}

set +e
DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd $DIR0
export LOGICMOO_WS=$DIR0

./logicmoo_env.sh .

INSTALL_BASE="$LOGICMOO_WS/lib/swipl"
if [ -d "$INSTALL_BASE" ]; then

    echo "#* "
    echo "#* GOOD: SWI-Prolog was installed so we wont try to APT update dependencies"
    echo "#*   (if there was a problem with them rm -rf ${INSTALL_BASE} and restart this script)"
    echo "#* "
else

echo "#* "
echo "#* Install deps..."
echo "#* "

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

#apt-add-repository -y ppa:swi-prolog/devel
#apt-get install -y swi-prolog elpa-ediprolog swi-prolog-java swi-prolog-odbc swi-prolog-bdb
#apt-get remove -y swi-prolog 
#apt-get -y install cmake ninja-build $(apt-cache depends swi-prolog-x | grep Depends | sed "s/.*ends:\//" | tr '\n' ' ')
#apt-get build-dep swi-prolog
# # default-jdk junit4 \
apt-get update
apt-get install -y \
        build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre3-dev \
        libyaml-dev \
	texlive

MAINTAINER apt-get install -y \
        texlive-latex-extra \
        texlive-font-utils \
        texlive-fonts-extra \
        texlive-fonts-extra-doc \
        texlive-fonts-recommended \
        texlive-fonts-recommended-doc

fi

  
(
cd $LOGICMOO_WS


INSTALL_BASE="$LOGICMOO_WS/lib/swipl"


if ! [ -d "$LOGICMOO_WS/swipl-devel" ]; then
( cd $LOGICMOO_WS
  git clone https://github.com/SWI-Prolog/swipl-devel.git swipl-devel
  cd swipl-devel  
  git submodule update --init
  patch -p1 --merge < /opt/logicmoo_workspace/Patch/dmiles-attvar.patch
  patch -p1 --merge < /opt/logicmoo_workspace/Patch/dmiles-save-reference-error.patch
  patch -p1 --merge < /opt/logicmoo_workspace/Patch/dmiles-no-sandbox.patch
  ( cd packages/ssl ; patch -p1 --merge < /opt/logicmoo_workspace/Patch/dmiles-ssl.patch )
)
fi

(
cd swipl-devel/
#git reset --hard HEAD
#git clean -f -x 
#git checkout origin/master . -f
git pull --recurse-submodules
git status -s 
)

MAKE=ninja
MAKE=make
(cd swipl-devel
 \cp -a ../Patch/rootfs/usr/local/lib/swipl/* .
 rm -rf build
 mkdir -p build
 cd build
 cmake -DCMAKE_INSTALL_PREFIX=$LOGICMOO_WS -G "Unix Makefiles" ..
 # cmake -DCMAKE_INSTALL_PREFIX=$LOGICMOO_WS -G Ninja .. 
 $MAKE -j 40 
 ctest -j 40

 if [ -d "$INSTALL_BASE" ]; then
  echo "#* "
  echo "#* rm -rf*ing swipl in ${INSTALL_BASE}..."
  echo "#* "
  rm -rf "$INSTALL_BASE"
 fi

 $MAKE install

 

 # rm -rf swipl-devel/build
)

find packs_* -name "*.qlf"
# swipl 

stty sane
echo "#* MAYBE cat .swiplrc >> ~/.config/swi-prolog/init.pl"


#mkdir -p bin/
#mkdir .local/share/swi-prolog/pack -p
#chmod 555 .local/share/swi-prolog/pack

)
stty sane

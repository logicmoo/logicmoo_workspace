#!/bin/bash -x

function MAINTAINER {
 echo SKIPPING: $*
}
function RUN {
 echo RUNing: $*
 $*
}

set +e

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mWARNING: This script ${BASH_SOURCE[0]} should be run as root. \e[0m"
   echo "#* "
   return 0 2>/dev/null
   exit 0
fi


mkdir -p /usr/share/man/man1

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

#git submodule init
#git submodule update
#git submodule sync --recursive
#git config --global http.sslVerify false
#git status -v --show-stash
#git pull -f && git pull -f --recurse-submodules

DIR="$LOGICMOO_WS/lib/deps_installed"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: Logicmoo Deps are hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
else
(source ./INSTALL-DEPS.md )
fi

stty sane

cd $DIR0
# who/where
export LOGICMOO_USER=prologmud_server
export LOGICMOO_WS=$DIR0
export LOGICMOO_GAMES=$LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games
. $DIR0/logicmoo_env.sh
(
cd $DIR0

echo whoami=`whoami`
echo PATH=$PATH
echo LOGICMOO_GAMES=$LOGICMOO_GAMES
echo LOGICMOO_WS=$LOGICMOO_WS
echo "127.0.0.1 eggdrop"  >> /etc/hosts

if [ ! -z "$LOGICMOO_EXTRAS" ];
 then
  curl -O http://mirror.umd.edu/eclipse/technology/epp/downloads/release/2020-06/R/eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz \
  && tar -zxvf eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz -C /usr/ \
  && ln -s /usr/eclipse/eclipse /usr/bin/eclipse \
  && rm -f eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz
 fi

#apt-add-repository -y ppa:swi-prolog/devel && apt-get install -y swi-prolog elpa-ediprolog swi-prolog-java swi-prolog-odbc swi-prolog-bdb

# check out our repo

if [[ ! -d /opt/logicmoo_workspace ]]; then
 mkdir -p /opt
 cd /opt 
 git config --global http.sslVerify false \
 echo "git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace"
 git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace 
 cd /opt/logicmoo_workspace
else
 cd /opt/logicmoo_workspace
 echo "git pull"
 git pull
fi

git config --local http.sslVerify false
echo "git submodule update --init"
git submodule update --init
echo "git pull --recurse-submodules"
git pull --recurse-submodules
#git lfs checkout
git submodule foreach 'git lfs fetch'
git submodule foreach 'git lfs pull'
git submodule foreach 'git lfs checkout .'
git submodule foreach 'git lfs checkout'

if [ ! -d /opt/logicmoo_workspace ]; then
mv /usr/local/lib/python3.6/ /usr/local/lib/python-DEAD-3.6
apt update 
apt install -y python3.7 python3.7-venv
echo ". /opt/logicmoo_workspace/packs_web/butterfly/bin/activate"
#. /opt/logicmoo_workspace/packs_web/butterfly/bin/activate
python3 -m pip install --upgrade pip
python3 -m pip install setuptools tornado
mv /usr/local/lib/python-DEAD-3.6/ /usr/local/lib/python3.6/ 
python3 -m pip install --upgrade pip
python3 -m pip install setuptools tornado
cd /opt/logicmoo_workspace/packs_web/butterfly
python3 -m pip install .
fi

rm -rf /usr/local/lib/python3.6/dist-packages/butterfly/templates
rm -rf /usr/local/lib/python3.6/dist-packages/butterfly/static

ln -s /opt/logicmoo_workspace/packs_web/butterfly/butterfly/static/ /usr/local/lib/python3.6/dist-packages/butterfly/static
ln -s /opt/logicmoo_workspace/packs_web/butterfly/butterfly/templates/ /usr/local/lib/python3.6/dist-packages/butterfly/templates

#\cp /opt/logicmoo_workspace/packs_web/butterfly/butterfly/static/* /usr/local/lib/python3.6/dist-packages/butterfly/static/
#\cp /opt/logicmoo_workspace/packs_web/butterfly/butterfly/templates/* /usr/local/lib/python3.6/dist-packages/butterfly/templates/

mkdir -p /opt/ros/eloquent/
touch /opt/ros/eloquent/setup.bash

# make our process running user
adduser --disabled-password --gecos "" --no-create-home $LOGICMOO_USER --home $LOGICMOO_GAMES
mkdir -p $LOGICMOO_GAMES/.config/nomicmu 
chown -R $LOGICMOO_USER $LOGICMOO_GAMES


# apache config
#cp -a -n -v $LOGICMOO_WS/packs_web/logicmoo_webui/etc/* /etc \
# ; cp -a -n -v $LOGICMOO_WS/packs_web/logicmoo_webui/var/* /var \
# ; cp -a -n -v $LOGICMOO_WS/etc/* /etc


# install swi-prolog
# DIR="$LOGICMOO_WS/swipl-devel"
DIR="$LOGICMOO_WS/lib/swipl"
if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: SWI-Prolog is hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
else
(cd $LOGICMOO_WS && ./INSTALL-SWI.md)
fi

RUN echo "copying config RC's into /root/"
# set up our runtime stuff (give root better shell stuff and our likely history commands)
MAINTAINER cp -n $LOGICMOO_GAMES/.??*rc ~root/ \
 ; cp -n $LOGICMOO_GAMES/.bash* ~root/ \
 ; cp -n $LOGICMOO_GAMES/.profile* ~root/ \

echo "Chowning and Chmoding..."
cd $LOGICMOO_WS \
 && touch $LOGICMOO_GAMES/history_4000 \
 && touch $LOGICMOO_GAMES/completion_4000 \
 && touch $LOGICMOO_GAMES/history_4001 \
 && touch $LOGICMOO_GAMES/completion_4001 \
 && touch $LOGICMOO_GAMES/history_4002 \
 && touch $LOGICMOO_GAMES/completion_4002 \
 && touch $LOGICMOO_GAMES/history_4003 \
 && touch $LOGICMOO_GAMES/completion_4003 \
 && touch $LOGICMOO_GAMES/history_4004 \
 && touch $LOGICMOO_GAMES/completion_4004 \
 && touch $LOGICMOO_GAMES/history_4025 \
 && touch $LOGICMOO_GAMES/completion_4025 \
 && touch $LOGICMOO_GAMES/nohup.out \
 && chmod 777 $LOGICMOO_GAMES/completion_* \
 && chmod 777 $LOGICMOO_GAMES/history_* \
 && chmod 777 $LOGICMOO_GAMES/nohup* \
 && chown $LOGICMOO_USER $LOGICMOO_GAMES/*0* \
 && chmod 777 /opt/logicmoo_workspace/packs_sys/*/ \
 && chmod 777 /opt/logicmoo_workspace/packs_sys/*/*/ \
 && chmod 777 /opt/logicmoo_workspace/packs_sys/*/*/*/ \
 && echo Maybe chown -R $LOGICMOO_USER $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata/ \
 && chown -R $LOGICMOO_USER $LOGICMOO_WS/packs_sys/eggdrop/ \
 && echo Maybe chown -R $LOGICMOO_USER $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/ \
 && echo Maybe chown -R $LOGICMOO_USER $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/src~/ \
 && echo Maybe chown -R $LOGICMOO_USER $LOGICMOO_WS/packs_web/butterfly
    
#git update-index --assume-unchanged $LOGICMOO_WS/packs_sys/eggdrop/conf/P*
#echo "${BASH_SOURCE[0]} Assesing GIT STATUS..."
#git status -s
)

find -name "*.qlf" -exec touch '{}' +


)

echo "${BASH_SOURCE[0]} Exiting."


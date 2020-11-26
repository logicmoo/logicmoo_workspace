#!/bin/bash

export PATH=/root/.cpm/bin:/opt/logicmoo_workspace/packs_xtra/logicmoo_packages/prolog/pakcs/bin:$PATH

export DISPLAY=10.0.0.122:0.0

if [[ ! -v SSH_TTY ]]; then
/bin/true
elif [[ -z "$SSH_TTY" ]]; then
/bin/true
else
 chmod o+rw $SSH_TTY
 echo chmod  o+rw $SSH_TTY
fi



if [[ -z "${LOGICMOO_WS}" ]]; then
 WS_MAYBE="$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)"

 if [[ -d "${WS_MAYBE}/packs_sys" ]]; then
  export LOGICMOO_WS=$WS_MAYBE
 else
  export LOGICMOO_WS=/opt/logicmoo_workspace
 fi

 echo LOGICMOO_WS=$LOGICMOO_WS

fi

if [[ ! "$PATH:" == "$LOGICMOO_WS/bin:"* ]]; then
 export PATH=$LOGICMOO_WS/bin:$PATH
fi

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  set -o ignoreeof
fi


if [[ -z "${JAVA_HOME}" ]]; then
 echo  #LIBJVM=(find $JAVA_HOME -name libjvm.so -printf "%h\n" | head -n 1)
 else
 echo  #/LIBJV=(find /usr/lib/jvm -name libjvm.so -printf "%h\n" | head -n 1)
fi

if [[ -z "${LD_LIBRARY_PATH}" ]]; then
    export LD_LIBRARY_PATH=/usr/lib
fi

if ! [[ ":$LD_LIBRARY_PATH:" == ":$LIBJVM"* ]]; then
 if [[ -z "${LD_LIBRARY_PATH}" ]]; then
    export LD_LIBRARY_PATH=$LIBJVM
 else
    export LD_LIBRARY_PATH=$LIBJVM:$LD_LIBRARY_PATH
 fi

 export BOOST_ROOT=$LOGICMOO_WS/taupl/boost_1_67_0
 export LD_LIBRARY_PATH=/usr/lib
 export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server:.:$BOOST_ROOT/lib:$LD_LIBRARY_PATH

 echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH
fi

echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH
echo PATH=$PATH




#!/bin/bash

if [[ -z "${LOGICMOO_WS}" ]]; then
 WS_MAYBE="$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)"

 if [[ -d "${WS_MAYBE}/packs_sys" ]]; then
  export LOGICMOO_WS=$WS_MAYBE
 else
  export LOGICMOO_WS=/opt/logicmoo_workspace
 fi

 echo "#* Set logicmoo workspace"
 echo "#* LOGICMOO_WS=$LOGICMOO_WS"

fi

if [[ ":$PATH:" == *"$LOGICMOO_WS/bin:"* ]]; then
   echo "#* "
   if [[ -z "$1" ]]; then
      echo "#* GOOD: Logicmoo [$LOGICMOO_WS/bin] found in your PATH"
   fi
else
 PATH="/root/.cpm/bin:/opt/logicmoo_workspace/packs_xtra/logicmoo_packages/prolog/pakcs/bin:$PATH"
 export PATH="$LOGICMOO_WS/bin:$PATH"
 echo "#* PATH=$PATH"
fi

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  set -o ignoreeof
fi

if [ -n "$DISPLAY" ] || [ -n "$DISPLAY" ]; then
   export DISPLAY=10.0.0.122:0.0
fi

if [[ -z "${LIBJVM}" ]]; then

   if [[ -z "${JAVA_HOME}" ]]; then
    export LIBJVM=$(find $JAVA_HOME -name libjvm.so -printf "%h\n" | head -n 1)
   else
    export LIBJVM=$(find /usr/lib/jvm -name libjvm.so -printf "%h\n" | head -n 1)
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
    echo "#* LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
   fi

fi




if [[ ! -v SSH_TTY ]]; then
/bin/true
elif [[ -z "$SSH_TTY" ]]; then
/bin/true
elif [[ -w "$SSH_TTY" ]]; then
/bin/true
else 
 echo "#* MAYBE: need to chmod  o+rw $SSH_TTY"
 chmod o+rw $SSH_TTY
fi





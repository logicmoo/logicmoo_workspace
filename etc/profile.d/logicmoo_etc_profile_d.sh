#!/bin/bash



export WS_MAYBE=$PWD

if [ -d "${LOGICMOO_WS}" ]; then
  echo export LOGICMOO_WS=$WS_MAYBE
else
  export LOGICMOO_WS=/opt/logicmoo_workspace
fi

if [ -d "${WS_MAYBE}/packs_sys" ]; then
  export LOGICMOO_WS=$WS_MAYBE
else
  export LOGICMOO_WS=/opt/logicmoo_workspace
fi



echo source $LOGICMOO_WS/logicmoo_env.sh
source $LOGICMOO_WS/logicmoo_env.sh


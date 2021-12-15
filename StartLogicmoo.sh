#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo ""
  #echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
  # return 1 2>/dev/null
  # exit 1
fi


DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
export LOGICMOO_WS=$DIR0
export LOGICMOO_GAMES=$LOGICMOO_WS/prologmud_server
(
   cd $DIR0

   if [ ! -f /.dockerenv ]; then
      container_name=logicmoo
      if docker ps -a --format '{{.Names}}' | grep -Eq "^${container_name}\$"; then
         docker exec -it logicmoo screen -rx LogicmooServer
      else
         bash ./runFromDocker.sh
      fi
     return 0 2>/dev/null
     exit 0
   fi

   ./logicmoo_env.sh
   echo LOGICMOO_GAMES=$LOGICMOO_GAMES
   echo LOGICMOO_WS=$LOGICMOO_WS
   # export SCREEN_CMD='sudo -u prologmud_server -- screen"
   sh -c "${LOGICMOO_WS}/logicmoo_env.sh ; . ${LOGICMOO_WS}/packs_web/butterfly/bin/activate ; export LOGICMOO_WS=$LOGICMOO_WS && cd ${LOGICMOO_GAMES} && ./StartMUD.sh $*"
)
stty sane
return 0 2>/dev/null
exit 0





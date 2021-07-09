#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi

GREEN='\033[0;32m'
NC='\033[0m' # No Color
function green {
    printf "${GREEN}$@${NC}\n"
}
function red {
    printf "${RED}$@${NC}\n"
}

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
# $DIR0/StartLogicmoo.sh


COUNTER=0

while [ 0 -lt 4 ]
do

let COUNTER+=1

export SCREEN_CMD="sudo -u prologmud_server -- screen"

if pgrep -x "screen" > /dev/nulli="0"
then
  if [ "$needs_message_update" != "0" ]; then
    echo "Screen Already Running"
    needs_message_update="0"
  fi
else
    echo "Screen not running"
    $SCREEN_CMD -mdS "LogicmooServer"
    sleep 2
    $SCREEN_CMD -S LogicmooServer -p0 -X stuff "$DIR0/StartLogicmoo.sh\r"
    sleep 2
    echo "Screen Started"

     if  pgrep -f "StartLogicmoo" > /dev/nulli="0"
      then
         if [ "$needs_message_update" != "0" ]; then
          echo "Looks like StartLogicmoo is running!"
          needs_message_update="0"
         fi
      else
          echo "Starting StartLogicmoo"
          needs_message_update="1"
          $SCREEN_CMD -S LogicmooServer -p0 -X stuff "$DIR0/StartLogicmoo.sh\r"
          sleep 2
      fi
fi

 if [ $COUNTER -gt 30 ]; then  
  echo "MAYBE (IN OTHER TERMINAL): " $(green "docker exec -it logicmoo $SCREEN_CMD -rx LogicmooServer")
  echo "OR (Bash IN OTHER TERMINAL): " $(green "docker exec -it logicmoo bash ")
  COUNTER=0
 fi

sleep 1

done


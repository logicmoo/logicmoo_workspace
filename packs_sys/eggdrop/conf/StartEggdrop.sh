#!/bin/bash

if pgrep -x "eggdrop" > /dev/null
then
    echo "Running"
    sleep 30
else
    echo "Eggdop not running"
    rm -f pid.*

if [[ $EUID -ne 0 ]]; then
 if ! [[ -f PrologMUD-freenode.user ]]; then
 eggdrop -n -m
  else
 eggdrop -n
 fi
else
   echo "#* "
   echo -e "\e[1;31mWarning: This script ( ${BASH_SOURCE[0]})  should not be run as root. \e[0m"
   echo "#* "
   sudo -u prologmud_server bash ${BASH_SOURCE[0]} $*
fi

fi

sleep 10


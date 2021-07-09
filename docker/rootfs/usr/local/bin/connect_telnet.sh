#!/bin/bash
set -o pipefail

if [[ $EUID -eq 0 ]]; then
   sudo -u prologmud_server -- ${BASH_SOURCE[0]} $@
   return 0 2>/dev/null
   exit 0
fi

echo PTTY=$PTTY

export PORT=4000

if [ $# -eq 0 ] 
 then
   echo "No arguments supplied thus using default port"
 else
   PORT=$1
fi

export WD=/opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games
export PATH=/usr/bin:$PATH

touch $WD/completion_$PORT
touch $WD/history_$PORT

function connect() {
   rlwrap -a -A -r -c -N -r --file=$WD/completion_$PORT --history-filename=$WD/history_$PORT -s 1111 telnet localhost $PORT
   return 0
}
LOOP=1

while [ $LOOP -eq 1 ]
do
 (
   set -e
   connect;
   LOOP=0
 )
 echo sleeping for 10 seconds
 sleep 10
done


return 0 2>/dev/null
exit 0

#!/bin/bash
set -o pipefail

#--help # Connect via Telnet

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

export WD=$LOGICMOO_WS/prologmud_server
export HIST_COMP=/tmp/tempDir
export PATH=/usr/bin:$PATH

touch $HIST_COMP/completion_$PORT
touch $HIST_COMP/history_$PORT

function connect() {
   rlwrap -a -A -r -c -N -r --file=$HIST_COMP/completion_$PORT --history-filename=$HIST_COMP/history_$PORT -s 1111 telnet localhost $PORT
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

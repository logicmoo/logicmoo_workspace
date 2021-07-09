#!/bin/bash

# BUTTERFLY_HOME=$LOGICMOO_WS/packs_web/butterfly
BUTTERFLY_HOME=$(cd $(dirname -- $0) && cd .. && cd packs_web/butterfly && pwd -P)
PORT=$2



if [ -z "$3" ]; then
   WEBPORT=$((100 + $2))
   lsof -t -i:$WEBPORT | xargs --no-run-if-empty kill -9
elif ! [[ "$3" =~ '^[0-9]+$' ]] ; then
   WEBPORT=$((100 + $2))
else 
   WEBPORT=$3
   lsof -t -i:$WEBPORT | xargs --no-run-if-empty kill -9
fi


export REDIR_NOW="${0} rlwrap ${PORT} ${WEBPORT}"
SUSER=$3
STASK=$4
BF_VEROPTS="--i_hereby_declare_i_dont_want_any_security_whatsoever --debug --unminified --unsecure"
#BF_VEROPTS="--debug"


if [ "${1}" = "kill" ]; then
 lsof -t -i:$WEBPORT | xargs --no-run-if-empty kill -9
elif [ "${1}" = "ttyd" ]; then
  ttyd -r 100 -p $WEBPORT $REDIR_NOW
elif [ "${1}" = "butterfly" ]; then   
   $BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --login=False --logging=debug --host=0.0.0.0 --port=$WEBPORT \
      --motd='../../../logicmoo.motd' --cmd="${REDIR_NOW}"

elif [ "${1}" = "dbutterfly" ]; then
   $BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --login=False --logging=debug --host=0.0.0.0 --port=$WEBPORT \
      --cmd="telnet -E -4 127.0.0.1 ${PORT}"

elif [ "${1}" = "remote" ]; then
   $BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --login=False --logging=debug --host=0.0.0.0 --port=$WEBPORT  --cmd="sudo -H -u ${SUSER} ${STASK}"

elif [ "${1}" = "shell" ]; then
   if [ -z "$STASK" ]; then
      su -c "$BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --login=False --logging=debug --host=0.0.0.0 --port=$WEBPORT --cmd='/bin/bash --login'" $SUSER
   else
      su -c "$BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --login=False --logging=debug --host=0.0.0.0 --port=$WEBPORT --cmd='${STASK}'" $SUSER
   fi

elif [ "${1}" = "login" ]; then
   $BUTTERFLY_HOME/butterfly.server.py ${BF_VEROPTS} --logging=debug --host=0.0.0.0 --port=$WEBPORT # --cmd='/bin/login'

elif [ "${1}" = "rlwrap" ]; then
  echo PORT=$PORT WEBPORT=$WEBPORT LOCATION=$LOCATION
  HIST=$BUTTERFLY_HOME/history_$PORT
  COMP=$BUTTERFLY_HOME/completion_$PORT
  touch $COMP $HIST
  rlwrap -a -A -r -c -N -r --file=${COMP} --history-filename=${HIST} -s 1000 telnet localhost $PORT

else
   echo "Error:" $0 $*
   echo ""
   echo "Usages: websocket_redir.sh <butterfly|rlwrap|kill|ttyd> InternalPort [WebPort]"
   echo "        websocket_redir.sh remote 1222 remote_prologmud_programmer emacs"
   echo "        websocket_redir.sh remote 1222 remote_prologmud_programmer 'ls -l'"
   echo '        websocket_redir.sh remote 1222 www-data "ps axf"'
   echo "        websocket_redir.sh remote 1222 www-data '/bin/bash --login'"   
   echo '        websocket_redir.sh shell 1222 remote_prologmud_programmer'
   echo "        websocket_redir.sh shell 1222 remote_prologmud_programmer 'ps axf'"
   echo '        websocket_redir.sh login 1222 '
     

   echo ""
fi



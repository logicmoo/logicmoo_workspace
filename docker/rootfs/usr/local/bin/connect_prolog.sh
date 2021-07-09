#!/bin/bash
set -e
export SCREEN_CMD="sudo -u prologmud_server -- screen"
#echo PTTY:TTY:PTS=$PTTY:$TTY:$PTS
#whoami

( 
  $SCREEN_CMD -S LogicmooServer -p0 -X stuff "${STUFF}"
)
echo $( 
$SCREEN_CMD -rx LogicmooServer
)
return 0 2>/dev/null
exit 0


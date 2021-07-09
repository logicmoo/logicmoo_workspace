#!/bin/bash +x

if [[ -f /startup.sh ]]
then
 ./startup.sh
else
 supervisord  -c /etc/supervisor/supervisord.conf
 $LOGICMOO_WS/StartLogicmoo.sh
fi


#!/bin/bash +x

if [[ -f $LOGICMOO_WS/docker/rootfs/startup.sh ]]
then
    $LOGICMOO_WS/docker/rootfs/startup.sh
   else
      if [[ -f /opt/logicmoo_workspace/docker/rootfs/startup.sh ]]
         then
          /opt/logicmoo_workspace/docker/rootfs/startup.sh
      else
            if [[ -f /startup.sh ]]
            then
             /startup.sh
            else
             supervisord  -c /etc/supervisor/supervisord.conf
             $LOGICMOO_WS/StartLogicmoo.sh
            fi
      fi
fi


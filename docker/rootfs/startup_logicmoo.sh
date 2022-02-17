#!/bin/bash +x


if [[ -f $LOGICMOO_WS/docker/rootfs/startup.sh ]]
then
   [ ! -f /finished_startup ] && $LOGICMOO_WS/docker/rootfs/startup.sh
   else
      if [[ -f /opt/logicmoo_workspace/docker/rootfs/startup.sh ]]
         then
          [ ! -f /finished_startup ] && /opt/logicmoo_workspace/docker/rootfs/startup.sh
      else
            if [[ -f /startup.sh ]]
            then
             [ ! -f /finished_startup ] && /startup.sh
            else
             supervisord  -c /etc/supervisor/supervisord.conf
             $LOGICMOO_WS/StartLogicmoo.sh
            fi
      fi
fi

exec tini -w -vv -- /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf


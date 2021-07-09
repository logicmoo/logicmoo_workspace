#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   return 0 2>/dev/null
   exit 0
fi


#start_oo_redir {
#sysctl net.ipv4.ip_forward=1
#iptables -F FORWARD
#iptables -t nat -A PREROUTING -p tcp --dport 58100 -j  REDIRECT --to-destination 10.0.0.122:8100
#iptables -A FORWARD -p tcp -d 10.0.0.122 --dport 8100 -m state --state NEW,ESTABLISHED,RELATED -j ACCEPT
##iptables -A POSTROUTING -t nat -p tcp -m tcp -s 10.0.0.122 --sport 8100 -j SNAT --to-source 10.0.0.180:58100
#iptables -t nat -A POSTROUTING -j MASQUERADE
#iptables -L
#/ ssh root@localhost -L 127.0.0.1:58100:10.0.0.122:8100
#}


export WS_MAYBE=$PWD

if [ -d "${LOGICMOO_WS}" ]; then
  echo export LOGICMOO_WS=$WS_MAYBE
else
  export LOGICMOO_WS=/opt/logicmoo_workspace
fi

if [ -d "${WS_MAYBE}/packs_sys" ]; then
  export LOGICMOO_WS=$WS_MAYBE
else
  export LOGICMOO_WS=/opt/logicmoo_workspace
fi


echo WHOAMI=`whoami`
echo source $LOGICMOO_WS/logicmoo_env.sh
source $LOGICMOO_WS/logicmoo_env.sh


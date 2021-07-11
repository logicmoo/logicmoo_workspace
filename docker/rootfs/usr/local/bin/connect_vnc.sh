#!/bin/bash
set -o pipefail

if [[ $EUID -eq 0 ]]; then
   sudo -u prologmud_server -- ${BASH_SOURCE[0]} $@
   return 0 2>/dev/null
   exit 0
fi

source /opt/logicmoo_workspace/logicmoo_env.sh

sudo -u prologmud_server -- google-chrome "http://localhost:6081/vnc.html"

return 0 2>/dev/null
exit 0

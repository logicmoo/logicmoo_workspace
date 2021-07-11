#!/bin/bash

#--help # Display LOGICMOO WWW in a web browser

if [ ! -f /.dockerenv ]; then
   docker exec -it logicmoo bin/$(basename "${BASH_SOURCE[0]}") $*
   return 0 2>/dev/null
   exit 0
fi

set -o pipefail

source /opt/logicmoo_workspace/logicmoo_env.sh

sudo -u prologmud_server -- google-chrome "http://localhost"

return 0 2>/dev/null
exit 0
